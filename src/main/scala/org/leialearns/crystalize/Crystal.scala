package org.leialearns.crystalize

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable

object Crystal {
  private val last: AtomicLong = new AtomicLong(0)
  private val map: mutable.Map[Location[_],History] = new mutable.HashMap[Location[_],History]()
  private val internalized: mutable.Map[Any,Any] = new mutable.HashMap[Any,Any]()
  def put[T <: Any](location: Location[T], value: T): Unit = {
    val time = last.incrementAndGet()
    val history = map.getOrElse(location, new History)
    if (!map.contains(location)) {
      map.put(location, history)
    }
    history.add(time, value)
  }
  def get[T <: Any](location: Location[T], time: Long): Option[T] = {
    map.get(location) flatMap ((history) => history.get(time)) map location.cast
  }
  def internalize[T](value: T): T = {
    val result = internalized.getOrElse(value, value)
    if (!internalized.contains(value)) {
      internalized.put(value, value)
    }
    value.getClass.cast(result)
  }
}
