package org.leialearns.crystallize.interaction

import grizzled.slf4j.Logging
import org.leialearns.crystallize.event.History
import org.leialearns.crystallize.item.{Item, Node}
import org.leialearns.crystallize.model.ItemCounts
import org.leialearns.crystallize.util.Marker

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success}

class Encounter(_history: History) extends Logging {
  info("Created encounter")

  val history: History = _history

  def run(responder: Actor, environment: Actor): Future[Unit] = {
    info("About to run encounter")
    Future {
      interact(responder: Actor, environment: Actor)
    }
  }

  def interact(responder: Actor, environment: Actor): Unit = {
    info("Running encounter")
    var state = Node.getNode(Actor.empty)
    var consolidated = Future.successful(Marker.MARKER)
    debug(s"Consolidated is completed: ${consolidated.isCompleted}")
    while (true) {
      val observation = environment.nextAction()
      debug(s"Observation: $observation")
      if (observation == Actor.stop) {
        info("Observation STOP")
        return
      }

      trace(s"State: $state")
      history.handleObserved(state, observation)
      trace(s"After handle observed: $state")


      if (observation != Actor.empty) {
        state = state.update(observation)
        responder.provideItem(observation)
      }
      val action = responder.nextAction()
      debug(s"Action: $action")
      if (action == Actor.stop) {
        info("Action STOP")
        return
      }
      if (action != Actor.empty) {
        state = state.update(observation)
        environment.provideItem(action)
      }
    }
  }
}
