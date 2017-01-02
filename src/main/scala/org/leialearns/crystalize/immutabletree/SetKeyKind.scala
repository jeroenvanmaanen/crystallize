package org.leialearns.crystalize.immutabletree

class SetKeyKind[A] extends KeyKind[A] {
  override def compare(one: A, other: A): Int = {
    val hashOne = if (one == null) 0 else one.hashCode()
    val hashOther = if (other == null) 0 else other.hashCode()
    hashOne - hashOther
  }

  override def equals(one: A, other: A): Boolean = {
    one == other
  }
}
