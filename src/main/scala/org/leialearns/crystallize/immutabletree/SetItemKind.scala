package org.leialearns.crystallize.immutabletree

trait SetItemKind[A] extends ItemKind[A, A, Unit] {
  override def getKey(item: A): A = item
  override def getValue(item: A): Unit = ()
  override def compare(one: A, other: A): Int = {
    val hashOne = if (one == null) 0 else one.hashCode()
    val hashOther = if (other == null) 0 else other.hashCode()
    hashOne - hashOther
  }
  override def equals(one: A, other: A): Boolean = {
    one == other
  }
}
