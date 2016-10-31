package org.leialearns.crystalize

import java.util.concurrent.atomic.{AtomicReference, AtomicLong}

import grizzled.slf4j.Logging
import org.leialearns.crystalize.item.{Category, Item, Node}

import scala.collection.mutable
import scala.concurrent.{Promise, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class Crystal(_propagators: Seq[Propagator]) extends Logging {
  private val last: AtomicLong = new AtomicLong(0)
  val propagators = _propagators
  val expectedItem = Item.getItem(Category.getCategory("expected"), ())
  val rootExpectedLocation = new AssignedLocation(Node.getNode(expectedItem), classOf[Boolean])
  var head: AtomicReference[State[_]] = new AtomicReference[State[_]](new State(None, "head", this, 0, rootExpectedLocation, Some(true)))

  def advance(newHead: State[_]): Boolean = {
    head.compareAndSet(newHead.previousStateOption.get, newHead)
  }

  def put[T <: Any](location: AssignedLocation[T], value: T): State[_] = {
    update(parent => {
      val time = last.incrementAndGet()
      new State[T](parent, "Observed", this, time, location, Some(value))
    })
  }

  def remove(location: AssignedLocation[_]): State[_] = {
    update(parent => {
      val time = last.incrementAndGet()
      new State(parent, "Observed", this, time, location, None)
    })
  }

  def get[T <: Any](location: AssignedLocation[T]): Future[T] = {
    head.get().get(location)
  }

  def getLast = last.get()

  def nullToOption[T <: Any](value: T): Option[T] = {
    if (value == null) None else Some(value)
  }

  def update(tryUpdate: (Option[State[_]]) => State[_]): State[_] = {
    val newHead = tryUpdate(nullToOption(head.get()))
    if (advance(newHead)) newHead else update(tryUpdate)
  }

  def update[T](location: AssignedLocation[T], zero: T, tryUpdate: T => T): Future[State[_]] = {
    val promise = Promise[State[_]]()
    update(location, zero, tryUpdate, promise)
    promise.future
  }

  protected def update[T](location: AssignedLocation[T], zero: T, tryUpdate: (T => T), promise: Promise[State[_]]): Unit = {
    val parent = head.get()
    parent.get(location).onComplete((t) => {
      val value: T = tryUpdate(t.getOrElse(zero))
      val newState = parent.put(location, value)
      if (advance(newState)) {
        newState.get(location) onSuccess { case newValue => debug(s"New value: ${newState.ordinal}: $location: $newValue") }
        promise.success(newState)
      } else {
        update(location, zero, tryUpdate)
      }
    })
  }

  implicit def tryToOption[T](attempt: Try[T]): Option[T] = {
    attempt match {
      case Success(value) => Some(value)
      case Failure(t) => None
    }
  }
}

object Crystal {
  private val internalized: mutable.Map[Any,Any] = new mutable.HashMap[Any,Any]()

  def internalize[T](value: T): T = {
    val result = internalized.getOrElse(value, value)
    if (!internalized.contains(value)) {
      internalized.put(value, value)
    }
    value.getClass.cast(result)
  }
}
