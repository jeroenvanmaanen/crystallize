package org.leialearns.crystallize.observations

import grizzled.slf4j.Logging
import java.util.concurrent.Semaphore
import org.leialearns.crystallize.event._
import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model._
import java.util.concurrent.atomic.AtomicReference

class Snapshotter(val history: History) extends Logging {
  val thread = new Thread(() => snapshotter())
  thread.setDaemon(true)
  val semaphore = new Semaphore(0)

  def start(): Unit = {
    info("Start snapshotter thread")
    thread.start()
  }

  def snapshotter(): Unit = {
    info("Start snapshotter")
    debug("Log level debug")
    do {
      Thread.sleep(10)
      semaphore.acquire()
      while (semaphore.tryAcquire()) {
        // drop permit
      }
      createSnapshot()
    } while(true)
  }

  def createSnapshot(): Unit = {
    val current = history.retrieveCurrentHistory()
    val previousSnapshot = current._1
    val previousModel = previousSnapshot._1
    val recentEvents = current._2
    var model = previousModel
    var previousEvent = previousSnapshot._2
    for (eventHandle <- recentEvents) {
      previousEvent.next = Some(eventHandle)
      previousEvent = eventHandle
      val updater: Model => Model = eventHandle.event match {
        case StartEvent() => identity
        case ObservedEvent(node, item) => applyObservedEvent(eventHandle.ordinal, node, item)
        case ExpectedNodeAddedEvent(node, referenceOrdinal) => applyExpectedNodeChangedEvent(referenceOrdinal, node, -1)
        case ExpectedNodeRemovedEvent(node, referenceOrdinal) => applyExpectedNodeChangedEvent(referenceOrdinal, node, 1)
        case ExpectedNodeVerifiedEvent(node, referenceOrdinal) => applyExpectedNodeChangedEvent(referenceOrdinal, node, 0)
      }
      model = updater(model)
    }
    if (model ne previousModel) {
      debug(s"Update snapshot: ${previousSnapshot._2.ordinal} -> ${previousEvent.ordinal}")
      history.lastSnapshot = (model, previousEvent)
      for (j <- previousSnapshot._2.ordinal until previousEvent.ordinal) {
        history.eventHandles.remove(j)
      }
    }
  }

  def applyObservedEvent(ordinal: Long, state: State, item: Item)(model: Model): Model = {
    var thisNodeValue: AbstractNodeValue = null
    thisNodeValue = getNodeValue(model, state).getOrElse(default = NodeValue(ordinal, ItemCounts.EMPTY))
    val oldCounts = thisNodeValue.counts
    val newCounts = oldCounts.increment(item, 1)
    val newNodeValue = if (thisNodeValue.isInExpectedModel) {
      NodeValueInExpectedModel(ordinal, newCounts)
    } else {
      NodeValue(ordinal, newCounts)
    }
    var result: Model = model + (state -> new AtomicReference(newNodeValue))
    if (!thisNodeValue.isInExpectedModel) {
      state.impliedStates().foreach {
        impliedState => result = applyObservedEvent(ordinal, impliedState, item)(result)
      }
    }
    if (!state.isExtensible()) {
      var first = 0l
      var second = 0l
      newNodeValue.counts.map.values.foreach {
        value =>
          if (value >= first) {
            second = first
            first = value
          } else if (value > second) {
            second = value
          }
      }
      if (second > 10) {
        state.markExtensible()
      }
    }
    result
  }

  def applyExpectedNodeChangedEvent(ordinal: Long, state: State, factor: Integer)(model: Model): Model = {
    var result = model
    getNodeValue(model, state) foreach {
      expectedNodeValue => state.impliedStates().foreach {
        impliedState => result = updateImpliedStates(ordinal, impliedState, factor, expectedNodeValue)(model)
      }
    }
    result
  }

  def updateImpliedStates(ordinal: Long, state: State, factor: Integer, expectedNodeValue: AbstractNodeValue)(model: Model): Model = {
    val thisNodeValue = getNodeValue(model, state).getOrElse(NodeValue(ordinal, ItemCounts.EMPTY))
    var counts = thisNodeValue.counts
    if (factor != 0) {
      thisNodeValue.counts.map.foreach {
        case (item, amount) => counts = counts.increment(item, factor * amount)
      }
    }
    var result = model
    val newNodeValue = if (thisNodeValue.isInExpectedModel) {
      NodeValueInExpectedModel(ordinal, counts)
    } else {
      state.impliedStates().foreach {
        impliedState => result = updateImpliedStates(ordinal, impliedState, factor, expectedNodeValue)(result)
      }
      NodeValue(ordinal, counts)
    }
    result + (state -> new AtomicReference(newNodeValue))
  }
}