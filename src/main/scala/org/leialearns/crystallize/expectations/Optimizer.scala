package org.leialearns.crystallize.expectations

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicLong

import grizzled.slf4j.Logging
import org.leialearns.crystallize.event.History._
import org.leialearns.crystallize.event._
import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.model.AbstractNodeValue
import org.leialearns.crystallize.util.Dump

class Optimizer(val history: History) extends Logging {
  val thread = new Thread(() => optimizer())
  thread.setDaemon(true)
  private val eventQueue = new LinkedBlockingQueue[EventHandle[State]]()
  private val queueSize = new AtomicLong(0l)

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
    eventHandle.event match {
      case ObservedEvent(state, item) =>
        val snapshot = history.lastSnapshot
        val current: Model = snapshot._1
        getNodeValue(current, state)
          .filter(_.updatedUpTo < eventHandle.ordinal)
          .map(optimize(snapshot, eventHandle, state, item, _))
          .map(history.addEvent)
      case _ => ()
    }
  }

  def optimize(snapshot: Snapshot, eventHandle: EventHandle[State], state: State, item: Item, nodeValue: AbstractNodeValue): Event[State] = {
    val counts = nodeValue.counts
    debug(s"Optimize: ${eventHandle.ordinal}: ${item}: ${state}: ${counts.total}")
    val expected = ItemCountsOptimizer.optimize(counts)
    if (isDebugEnabled) {
      Dump.dump("Expected", expected) foreach (debug(_))
    }
    Thread.sleep(100)
    ExpectedNodeVerifiedEvent(state, snapshot._2.ordinal)
  }
}