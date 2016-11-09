package org.leialearns.crystalize.util

object Marker {
  val MARKER = new Marker()
}

final class Marker {

  override def hashCode(): Int = 37

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[Marker]
  }

  override def toString: String = "Marker"
}
