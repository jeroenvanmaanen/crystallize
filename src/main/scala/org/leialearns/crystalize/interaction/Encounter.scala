package org.leialearns.crystalize.interaction

import grizzled.slf4j.Logging
import org.leialearns.crystalize.Crystal
import org.leialearns.crystalize.item.{Item, Node}
import org.leialearns.crystalize.model.{ItemCounts, Extensible, MaxDepth, Observed}

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success}

class Encounter(_crystal: Crystal) extends Logging {
  info("Created encounter")

  val crystal = _crystal

  def run(responder: Actor, environment: Actor): Future[Unit] = {
    info("About to run encounter")
    Future {
      interact(responder: Actor, environment: Actor)
    }
  }

  def interact(responder: Actor, environment: Actor): Unit = {
    info("Running encounter")
    var maxDepth = 1l
    var maxDepthFuture: Future[java.lang.Long] = crystal.head.get().get(MaxDepth.MAX_DEPTH_LOCATION)
    var state = immutable.Queue[Item]()
    while (true) {
      val observation = environment.nextAction()
      info(s"Observation: $observation")
      if (observation == Actor.stop) {
        info("Observation STOP")
        return
      }

      var nodeOption: Option[Node] = None
      for (item <- state.reverse) {
        nodeOption = Some(Node.getNode(nodeOption, item))
      }
      trace(s"Node option: $nodeOption")
      nodeOption match {
        case Some(node) =>
          trace(s"Update observed: $node: $observation")
          updateObserved(node, observation)
        case _ => ()
      }
      trace(s"After node option: $nodeOption")

      if (observation != Actor.empty) {
        state = state.enqueue(observation)
        responder.provideItem(observation)
      }
      val action = responder.nextAction()
      info(s"Action: $action")
      if (action == Actor.stop) {
        info("Action STOP")
        return
      }
      if (action != Actor.empty) {
        state = state.enqueue(action)
        environment.provideItem(action)
      }
      if (maxDepthFuture.isCompleted) {
        val newMaxDepth: Long = maxDepthFuture.value match {
          case Some(Success(value)) => value
          case _ => java.lang.Long.valueOf(0)
        }
        maxDepth = maxDepth max newMaxDepth
        debug(s"New max depth: $maxDepth")
        maxDepthFuture = crystal.head.get().get(MaxDepth.MAX_DEPTH_LOCATION)
      }
      val maxStateLength = maxDepth + 1
      while (state.length > maxStateLength) {
        state = state.dequeue._2
      }
    }
  }

  def updateObserved(node: Node, observation: Item): Unit = {
    node.parent match {
      case Some(parentNode) =>
        val parentExtensible = Extensible.createExtensibleLocation(parentNode)
        crystal.head.get().get(parentExtensible) onComplete {
          case Success(_) => updateNode(node, observation)
          case Failure(_) => updateObserved(parentNode, observation)
        }
      case None => updateNode(node, observation)
    }
  }

  def updateNode(node: Node, observation: Item): Unit = {
    val observedLocation = Observed.createObservedLocation(node)
    trace(s"Update node: $node, $observation")
    crystal.update(observedLocation, new ItemCounts(), (itemCounts: ItemCounts) => itemCounts.increment(observation, 1l)) onSuccess {
      case state => state.get(observedLocation) onSuccess {
        case itemCounts => trace(s"Update result: $observedLocation[$observation] -> ${itemCounts.get(observation)} (total: ${itemCounts.total}})");
      }
    }
  }
}
