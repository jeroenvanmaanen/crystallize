package org.leialearns.crystalize.immutabletree

trait MapItemKind[K, V] extends ItemKind[(K, V), K, V] {
  override def getKey(item: (K, V)): K = item._1
  override def getValue(item: (K, V)): V = item._2
  override def compare(one: K, other: K): Int = {
    val hashOne = if (one == null) 0 else getKeyHashCode(one)
    val hashOther = if (other == null) 0 else getKeyHashCode(other)
    hashOne - hashOther
  }
  override def equals(one: K, other: K): Boolean = {
    one == other
  }
  def getKeyHashCode(key: K): Int = key.hashCode()
}
