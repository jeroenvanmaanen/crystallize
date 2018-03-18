package org.leialearns.crystallize.util

class OrderedRational(_r: Rational, _limit: Long, _ordinal: Long) extends Comparable[OrderedRational] with DumpCustom {
  val r = _r;
  val limit = _limit
  val ordinal = _ordinal
  def compareTo(other: OrderedRational): Int = {
    r.compareTo(other.r)
  }
  override def dumpAs: Iterable[_] = {
    Iterable(ordinal, limit, r)
  }
}