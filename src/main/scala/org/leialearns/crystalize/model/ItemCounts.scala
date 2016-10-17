package org.leialearns.crystalize.model

import scala.collection.immutable

import org.leialearns.crystalize.item.Item

class ItemCounts(_map: immutable.HashMap[Item,Long], _total: Long) {
  def this() = {
    this(new immutable.HashMap[Item,Long](), 0l)
  }
  val map = _map
  val total = _total

  def get(item: Item): Long = map.getOrElse(item, 0)

  def increment(item: Item, amount: Long): ItemCounts = {
    val oldValue: Long = map.getOrElse(item, 0l)
    val newValue = oldValue + amount
    if (newValue < 0) {
      throw new IllegalStateException("Count underflow: " + oldValue + ": " + amount)
    }
    new ItemCounts(map + ((item, oldValue + amount)), total + amount)
  }

  override def toString = {
    s"[IC:${map.keySet.size}:$total]"
  }
}
