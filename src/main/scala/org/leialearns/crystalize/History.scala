package org.leialearns.crystalize

import scala.collection.mutable

class History {
  val values = new mutable.ArrayBuffer[(Long, Option[Any])]()

  def add(time: Long, value: Any): Unit = {
    if (values.nonEmpty && values.last._1 >= time) {
      throw new IllegalArgumentException(s"Time [$time] must be greater than last time [${values.last._1}]")
    }
    values += ((time, Some(value)))
  }

  def remove(time: Long): Unit = {
    if (values.nonEmpty && values.last._1 >= time) {
      throw new IllegalArgumentException(s"Time [$time] must be greater than last time [${values.last._1}]")
    }
    values += ((time, None))
  }

  def get(time: Long): Option[Any] = {
    var i = values.length - 1
    var result: Option[Any] = None
    while (i >= 0) {
      val current = values(i)
      result = current._2
      if (time >= current._1) {
        return result
      }
      i -= 1;
    }
    None
  }
}
