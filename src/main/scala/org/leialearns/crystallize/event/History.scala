package org.leialearns.crystallize.event

import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import scala.collection.immutable.HashMap

import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model.AbstractNodeValue
import grizzled.slf4j.Logging
import scala.collection.immutable.Queue
import org.leialearns.crystallize.observations.Snapshotter

class History extends Logging {
  val eventHandles: Map[Long,EventHandle[State]] = new ConcurrentHashMap[Long,EventHandle[State]]
  val startEventHandle = new EventHandle[State](0L, StartEvent())
  eventHandles.put(startEventHandle.ordinal, startEventHandle)
  var lastSnapshot: Snapshot = (HashMap.empty, startEventHandle)
  val newest: AtomicLong = new AtomicLong(0l)
  val snapshotThread = new Snapshotter(this)
  snapshotThread.start()

  def handleObserved(node: State, item: Item) {
    debug(s"Handle observed: ${node} -> ${item}")
    val ordinal = newest.incrementAndGet()
    val event = new ObservedEvent[State](node, item)
    val eventHandle = new EventHandle(ordinal, event)
    eventHandles.put(ordinal, eventHandle)
    snapshotThread.snapshotSemaphore.release();
  }

  def getCurrent(): (Snapshot, Queue[EventHandle[State]]) = {
    val snapshot = lastSnapshot
    var currentEventHandles = Queue[EventHandle[State]]()
    var eventHandle = snapshot._2
    var go = true
    var i = eventHandle.ordinal
    var newest = i
    do {
      newest = i
      i += 1
      eventHandle = eventHandle.next.getOrElse(null)
      if (eventHandle == null) {
        eventHandle = eventHandles.get(i)
      }
      if (eventHandle == null) {
        go = false
      } else {
        currentEventHandles = currentEventHandles :+ eventHandle
      }
    } while (go)
    (snapshot, currentEventHandles)
  }
}
object History {
  type Model = HashMap[State,AbstractNodeValue]
  type Snapshot = (Model, EventHandle[State])
}
