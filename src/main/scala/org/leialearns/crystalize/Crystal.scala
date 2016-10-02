package org.leialearns.crystalize

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable

object Crystal {
  private val last: AtomicLong = new AtomicLong(0)
  private val internalized: mutable.Map[Any,Any] = new mutable.HashMap[Any,Any]()
  var head: State[_] = new State(None, "head", 0, new Location(None, Unit.getClass), None)

  def advance(newHead: State[_]): State[_] = {
    if (newHead.previousStateOption == Some(head)) {
      head = newHead
    }
    head
  }

  def put[T <: Any](location: Location[T], value: T): State[_] = {
    val time = last.incrementAndGet()
    val newHead = new State[T](Some(head), "Observed", time, location, Some(value))
    advance(newHead)
  }

  def remove(location: Location[_]): State[_] = {
    val time = last.incrementAndGet()
    val newHead = new State(Some(head), "Observed", time, location, None)
    advance(newHead)
  }

  def get[T <: Any](location: Location[T], state: State[_]): Option[T] = {
    state.get(location) match {
      case Some(valueOption) => valueOption
      case _ => None
    }
  }

  def get[T <: Any](location: Location[T]): Option[T] = {
    get(location, head)
  }

  def internalize[T](value: T): T = {
    val result = internalized.getOrElse(value, value)
    if (!internalized.contains(value)) {
      internalized.put(value, value)
    }
    value.getClass.cast(result)
  }

  def getLast = last.get()
}
