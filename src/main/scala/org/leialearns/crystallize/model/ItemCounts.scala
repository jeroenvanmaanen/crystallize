package org.leialearns.crystallize.model

import org.leialearns.crystallize.util.DumpCustom

import scala.collection.immutable

import org.leialearns.crystallize.item.Item

class ItemCounts(val map: immutable.Map[Item,Long], val total: Long) extends DumpCustom {
  def this() = {
    this(new immutable.HashMap[Item,Long](), 0l)
  }

  def get(item: Item): Long = map.getOrElse(item, 0)

  def increment(item: Item, amount: Long): ItemCounts = {
    val oldValue: Long = map.getOrElse(item, 0l)
    val newValue = oldValue + amount
    if (newValue < 0) {
      throw new IllegalStateException("Count underflow: " + oldValue + ": " + amount)
    }
    new ItemCounts(map + ((item, oldValue + amount)), total + amount)
  }

  override def toString: String = {
    s"[IC:${map.keySet.size}:$total]"
  }

  override def dumpAs: Iterable[_] = {
    immutable.HashMap[String,AnyRef]() +
      ("map" -> map) +
      ("total" -> total)
  }
}
object ItemCounts {
  val empty = new ItemCounts(immutable.HashMap.empty, 0)
}
