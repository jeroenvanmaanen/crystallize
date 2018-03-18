package org.leialearns.crystallize.util

class Rational(_numerator: Long, _denominator: Long) extends Comparable[Rational] with DumpCustom {
  implicit object RationalIsFractional extends RationalIsFractional
  val numerator = _numerator
  val denominator = _denominator
  def compareTo(other: Rational): Int = {
    RationalIsFractional.compare(this, other)
  }
  override def dumpAs: Iterable[_] = {
    Iterable(s"(${numerator}/${denominator})")
  }
}
