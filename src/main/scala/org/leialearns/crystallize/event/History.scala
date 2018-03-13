package org.leialearns.crystallize.event

import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicLong

import scala.collection.immutable.HashMap

import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model.AbstractNodeValue
import org.leialearns.crystallize.model.ItemCounts
import org.leialearns.crystallize.model.NodeValue
import org.leialearns.crystallize.model.NodeValueInExpectedModel
import grizzled.slf4j.Logging

class History extends Logging {
  type Model = HashMap[State,AbstractNodeValue]
  val eventHandles: Map[Long,EventHandle[State]] = new ConcurrentHashMap[Long,EventHandle[State]]
  var lastSnapshot: (Long, Model, EventHandle[State]) = (0l, HashMap.empty, new EventHandle(StartEvent()))
  eventHandles.put(lastSnapshot._1, lastSnapshot._3)
  val newest: AtomicLong = new AtomicLong(0l)
  val snapshotSemaphore = new Semaphore(0)
  val snapshotThread = new Thread((() => snapshotter()))
  snapshotThread.setDaemon(true)
  snapshotThread.start()

  def handleObserved(node: State, item: Item) {
    debug(s"Handle observed: ${node} -> ${item}")
    val ordinal = newest.incrementAndGet()
    val event = new ObservedEvent[State](node, item)
    val eventHandle = new EventHandle(event)
    eventHandles.put(ordinal, eventHandle)
    snapshotSemaphore.release();
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
    var previousSnapshot = lastSnapshot
    var i = previousSnapshot._1
    val previousModel = previousSnapshot._2
    var model = previousModel
    var go = true
    var newest = i
    do {
      newest = i
      i += 1
      val eventHandle = eventHandles.get(i)
      if (eventHandle == null) {
        go = false
      } else {
        eventHandles.get(newest).next = Some(eventHandle)
        val updater: Model => Model = eventHandle.event match {
          case StartEvent() => identity
          case ObservedEvent(node, item) => applyObservedEvent(node, item)
          case ExpectedNodeAddedEvent(node) => applyExpectedNodeChangedEvent(node, -1)
          case ExpectedNodeRemovedEvent(node) => applyExpectedNodeChangedEvent(node, 1)
        }
        model = updater(model)
      }
    } while (go);
    if (model ne previousModel) {
      debug(s"Update snapshot: ${previousSnapshot._1} -> ${newest}")
      lastSnapshot = (newest, model, eventHandles.get(newest))
      for (j <- previousSnapshot._1 until newest) {
        eventHandles.remove(j)
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
