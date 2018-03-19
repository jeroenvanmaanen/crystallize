package org.leialearns.crystallize.observations

import grizzled.slf4j.Logging
import java.util.concurrent.Semaphore
import org.leialearns.crystallize.event._
import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model._

class Snapshotter(_history: History) extends Logging {
  val history = _history
  val thread = new Thread((() => snapshotter()))
  thread.setDaemon(true)
  val snapshotSemaphore = new Semaphore(0)

  def start() = {
    info("Start snapshotter thread")
    thread.start()
  }

  def snapshotter() {
    info("Start snapshotter")
    debug("Log level debug")
    do {
      Thread.sleep(10)
      snapshotSemaphore.acquire();
      while (snapshotSemaphore.tryAcquire()) {
        // drop permit
      }
      createSnapshot()
    } while(true);
  }

  def createSnapshot() {
    val current = history.getCurrent()
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
        case ObservedEvent(node, item) => applyObservedEvent(node, item)
        case ExpectedNodeAddedEvent(node) => applyExpectedNodeChangedEvent(node, -1)
        case ExpectedNodeRemovedEvent(node) => applyExpectedNodeChangedEvent(node, 1)
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

  def applyObservedEvent(state: State, item: Item)(model: Model): Model = {
    var thisNodeValue: AbstractNodeValue = null
    thisNodeValue = model.get(state).getOrElse(new NodeValue(ItemCounts.EMPTY))
    val oldCounts = thisNodeValue.counts
    val newCounts = oldCounts.increment(item, 1)
    val newNodeValue = if (thisNodeValue.isInExpectedModel) {
      new NodeValueInExpectedModel(newCounts)
    } else {
      new NodeValue(newCounts)
    }
    var result = model + (state -> newNodeValue)
    if (!thisNodeValue.isInExpectedModel) {
      state.impliedStates().foreach {
        impliedState => result = applyObservedEvent(impliedState, item)(result)
      }
    }
    if (!state.isExtensible()) {
      var first = 0l;
      var second = 0l;
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

  def applyExpectedNodeChangedEvent(state: State, factor: Integer)(model: Model): Model = {
    var result = model
    model.get(state) map {
      expectedNodeValue => state.impliedStates().foreach {
        impliedState => result = updateImpliedStates(impliedState, factor, expectedNodeValue)(model)
      }
    }
    result
  }

  def updateImpliedStates(state: State, factor: Integer, expectedNodeValue: AbstractNodeValue)(model: Model): Model = {
    val thisNodeValue = model.get(state).getOrElse(new NodeValue(ItemCounts.EMPTY))
    var counts = thisNodeValue.counts
    thisNodeValue.counts.map.foreach {
      case (item, amount) => counts = counts.increment(item, factor * amount)
    }
    var result = model
    val newNodeValue = if (thisNodeValue.isInExpectedModel) {
      new NodeValueInExpectedModel(counts)
    } else {
      state.impliedStates().foreach {
        impliedState => result = updateImpliedStates(impliedState, factor, expectedNodeValue)(result)
      }
      new NodeValue(counts)
    }
    result + (state -> newNodeValue)
  }
}