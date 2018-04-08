package org.leialearns.crystallize.expectations

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicLong

import grizzled.slf4j.Logging
import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.event._
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model.{AbstractNodeValue, NodeValueInExpectedModel}
import org.leialearns.crystallize.util.{Dump, RationalIsFractional}

import scala.math.log

class Optimizer(val history: History) extends Logging {
  val thread = new Thread(() => optimizer())
  thread.setDaemon(true)
  private val eventQueue = new LinkedBlockingQueue[EventHandle[State]]()
  private val queueSize = new AtomicLong(0l)
  implicit object RationalIsFractional extends RationalIsFractional
  private val ln2: Double = log(2)

  def start(): Unit = {
    info("Start optimizer thread")
    thread.start()
  }

  def put(eventHandle: EventHandle[State]): Unit = {
    eventQueue.put(eventHandle)
    queueSize.incrementAndGet()
  }

  def size(): Long = {
    queueSize.get()
  }

  def optimizer(): Unit = {
    info("Start optimizer")
    debug("Log level debug")
    do {
      consumeEvent()
    } while(true)
  }

  def consumeEvent(): Unit = {
    val eventHandle = eventQueue.take()
    queueSize.decrementAndGet()
    val resultOption =
      eventHandle.event match {
        case event if event.isInstanceOf[ObservedEvent] || event.isInstanceOf[ParentReevaluatedEvent] =>
          reevaluateState(event.state, eventHandle.ordinal).map((event.state, _))
        case _ => None
      }
    resultOption.foreach {
      case (state, event) =>
        history.addEvent(event)
        if (!event.isInstanceOf[ExpectedNodeAddedEvent]) {
          state.impliedStates().foreach(
            implied => history.addEvent(ParentReevaluatedEvent(implied, eventHandle.ordinal))
          )
        }
    }
  }

  def reevaluateState(state: State, ordinal: Long): Option[Event[State]] = {
    val snapshot = history.lastSnapshot
    val current: Model = snapshot._1
    val snapshotOrdinal: Long = snapshot._2.ordinal
    val nodeValues = getNodeValues(current, state)
    nodeValues
      .get(state)
      .filter(_.updatedUpTo < ordinal)
      .map(nodeValue => {
        val ancestorMap = nodeValues - state
        if (ancestorMap.isEmpty) {
          if ((!nodeValue.isInExpectedModel) && state.impliedStates().isEmpty) {
            ExpectedNodeAddedEvent(state, snapshotOrdinal)
          } else {
            ExpectedNodeVerifiedEvent(state, 0)
          }
        } else {
          val totalGain = (nodeValues - state).values.map(gain(snapshotOrdinal, state, nodeValue, _)).sum
          if (totalGain > 0.0) {
            if (nodeValue.isInExpectedModel) {
              ExpectedNodeRemovedEvent(state, snapshotOrdinal)
            } else {
              ExpectedNodeAddedEvent(state, snapshotOrdinal)
            }
          } else {
            ExpectedNodeVerifiedEvent(state, snapshotOrdinal)
          }
        }
      })
  }

  def gain(ordinal: Long, state: State, nodeValue: AbstractNodeValue, ancestor: AbstractNodeValue): Double = {
    val ancestorKeys = ancestor.counts.map.keys
    val oldAncestorProbabilities = optimize(ordinal, ancestor, ancestorKeys)
    val descendantProbabilities = optimize(ordinal, nodeValue, ancestorKeys)
    val oldAncestorData = ancestor.counts
    val newAncestorCounts =
      if (nodeValue.isInExpectedModel) {
        oldAncestorData.plus(nodeValue.counts)
      } else {
        oldAncestorData.minus(nodeValue.counts)
      }
    val newAncestorData = NodeValueInExpectedModel(ancestor.updatedUpTo, newAncestorCounts)
    val newAncestorProbabilities = optimize(ordinal, newAncestorData, ancestorKeys)
    val oldAncestorDescriptionLength = descriptionLength(oldAncestorProbabilities, ancestor)
    val newAncestorDescriptionLength = descriptionLength(newAncestorProbabilities, newAncestorData)
    val descendantDescriptionLength = descriptionLength(descendantProbabilities, nodeValue)
    val (oldDescriptionLength, newDescriptionLength) =
      if (nodeValue.isInExpectedModel) {
        (oldAncestorDescriptionLength + descendantDescriptionLength, newAncestorDescriptionLength)
      } else {
        (oldAncestorDescriptionLength, descendantDescriptionLength + newAncestorDescriptionLength)
      }
    oldDescriptionLength - newDescriptionLength
  }

  def descriptionLength(probabilities: Probabilities, nodeValue: AbstractNodeValue): Double = {
    nodeValue.counts.map.foldLeft(0.0) {
      case (a, (item, amount)) =>
        val p: Double = RationalIsFractional.toDouble(probabilities.map(item)._1)
        a + (amount * -logBase2(p))
    }
  }

  def logBase2(x: Double): Double = {
    log(x)/ln2
  }

  def optimize(ordinal: Long, nodeValue: AbstractNodeValue, ancestorItems: Traversable[Item]): Probabilities = {
    val counts = nodeValue.counts
    debug(s"Optimize: ${ordinal}: ${counts.total}")
    val expected = ItemCountsOptimizer.optimize(counts)
    if (isDebugEnabled) {
      Dump.dump("Expected", expected) foreach (debug(_))
    }
    expected
  }

  def getNodeValues(model: Model, state: State): Map[State, AbstractNodeValue] = {
    getNodeValue(model, state).map(
      nodeValue =>
        state.impliedStates().foldLeft(Map(state -> nodeValue)) {
          (map, impliedState) => {
            getImpliedAncestors(map, model, impliedState)
          }
        }
    ).getOrElse(Map.empty)
  }

  def getImpliedAncestors(map: Map[State, AbstractNodeValue], model: Model, state: State): Map[State, AbstractNodeValue] = {
    getNodeValue(model, state).map {
      nodeValue =>
        if (nodeValue.isInExpectedModel) {
          map + (state -> nodeValue)
        } else {
          state.impliedStates().foldLeft(map) {
            (map, impliedState) => {
              getImpliedAncestors(map, model, impliedState)
            }
          }
        }
    }.getOrElse(map)
  }
}