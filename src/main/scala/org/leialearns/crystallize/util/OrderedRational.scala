package org.leialearns.crystallize.util

class OrderedRational(val r: Rational, val limit: Long, val ordinal: Long) extends Comparable[OrderedRational] with DumpCustom {
  def compareTo(other: OrderedRational): Int = {
    r.compareTo(other.r)
  }
  override def dumpAs: Iterable[_] = {
    Iterable(ordinal, limit, r)
  }
  override def toString: String = {
    s"{${r}<${limit}#${ordinal}}"
  }
}