package org.leialearns.crystallize.immutabletree

trait MapKeyKind[K] extends KeyKind[K] {
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
