package org.leialearns.crystallize.util

class OrderedKey(override val sortKey: String, val value: Any) extends Sortable {
  override def toString: String = value.toString
}
