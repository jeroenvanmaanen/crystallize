package org.leialearns.crystallize.util

import Rational._

class Rational(_numerator: Long, _denominator: Long) extends Comparable[Rational] with DumpCustom {
  val numerator = _numerator
  val denominator = _denominator
  def +(rhs: Rational): Rational = RationalIsFractional.plus(this, rhs)
  def -(rhs: Rational): Rational = RationalIsFractional.minus(this, rhs)
  def *(rhs: Rational): Rational = RationalIsFractional.times(this, rhs)
  def /(rhs: Rational): Rational = RationalIsFractional.div(this, rhs)
  def <(rhs: Rational): Boolean = this.compareTo(rhs) < 0
  def >(rhs: Rational): Boolean = this.compareTo(rhs) > 0
  def <=(rhs: Rational): Boolean = this.compareTo(rhs) <= 0
  def >=(rhs: Rational): Boolean = this.compareTo(rhs) >= 0
  def compareTo(other: Rational): Int = {
    RationalIsFractional.compare(this, other)
  }
  override def dumpAs: Iterable[_] = {
    Iterable(s"(${numerator}/${denominator})")
  }
  override def toString: String = {
    s"(${numerator} / ${denominator})"
  }
  override def equals(other: Any): Boolean = {
    other match {
      case otherRational: Rational => compareTo(otherRational) == 0
      case _ => false
    }
  }
  override def hashCode: Int = {
    31 * (31 + numerator.toInt) + denominator.toInt
  }
}
object Rational {
  implicit object RationalIsFractional extends RationalIsFractional
  def apply(n: Long, d: Long): Rational = new Rational(n, d)
  def apply(n: Long): Rational = new Rational(n, 1)
}