package org.leialearns.crystallize.expectations

import grizzled.slf4j.Logging
import org.leialearns.crystallize.event._
import org.leialearns.crystallize.event.History._
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicLong

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
      case ObservedEvent(node, item) =>
        val snapshot = history.lastSnapshot
        val current: Model = snapshot._1
        if (getNodeValue(current, node) forall (_.updatedUpTo < eventHandle.ordinal)) {
          debug(s"Optimize: ${eventHandle.ordinal}: ${item}: ${node}")
          val event = ExpectedNodeVerifiedEvent(node, snapshot._2.ordinal)
          history.addEvent(event)
          Thread.sleep(100)
        }
      case _ => ()
    }
  }
}