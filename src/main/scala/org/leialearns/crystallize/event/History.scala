package org.leialearns.crystallize.event

import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import grizzled.slf4j.Logging
import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.expectations.Optimizer
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model.AbstractNodeValue
import org.leialearns.crystallize.observations.Snapshotter

import scala.collection.immutable.{HashMap, Queue}

class History extends Logging {
  val eventHandles: util.Map[Long,EventHandle[State]] = new ConcurrentHashMap[Long,EventHandle[State]]
  val startEventHandle = new EventHandle[State](0L, StartEvent())
  eventHandles.put(startEventHandle.ordinal, startEventHandle)
  var lastSnapshot: Snapshot = (HashMap.empty, startEventHandle)
  val newest: AtomicLong = new AtomicLong(0l)
  val snapshotter = new Snapshotter(this)
  snapshotter.start()
  val optimizer = new Optimizer(this)
  optimizer.start()
  var sleepMillis = 10.0
  val sleepFactor = 1.005

  def handleObserved(node: State, item: Item) {
    debug(s"Handle observed: ${node} -> ${item}")
    val event = new ObservedEvent[State](node, item)
    val eventHandle = addEvent(event)
    snapshotter.semaphore.release()
    optimizer.put(eventHandle)
    val queueSize = optimizer.size()
    if (queueSize > 100) {
      sleepMillis *= sleepFactor
    } else {
      sleepMillis /= sleepFactor
    }
    val currentMillis = sleepMillis.toLong
    debug(s"Sleep: ${currentMillis} (${queueSize})")
    Thread.sleep(currentMillis)
  }

  def addEvent(event: Event[State]): EventHandle[State] = {
    val ordinal = newest.incrementAndGet()
    debug(s"Add event: ${event.getClass.getSimpleName}: ${event.state} (#${ordinal})")
    val eventHandle = new EventHandle(ordinal, event)
    eventHandles.put(ordinal, eventHandle)
    eventHandle
  }

  def retrieveCurrentHistory(): (Snapshot, Queue[EventHandle[State]]) = {
    val snapshot = lastSnapshot
    var currentEventHandles = Queue[EventHandle[State]]()
    var eventHandle = snapshot._2
    var go = true
    var i = eventHandle.ordinal
    var newest = i
    do {
      newest = i
      i += 1
      eventHandle = eventHandle.next.orNull
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
  type Model = HashMap[State,AtomicReference[AbstractNodeValue]]
  type Snapshot = (Model, EventHandle[State])
  def getNodeValue(model: Model, state: State): Option[AbstractNodeValue] = {
    model.get(state) flatMap (reference => Option(reference.get))
  }
}
