package org.leialearns.crystallize.util

class OrderedKey(_sortKey: String, _value: Any) extends Sortable {
  override def toString = _value.toString
  override def sortKey = _sortKey
}
