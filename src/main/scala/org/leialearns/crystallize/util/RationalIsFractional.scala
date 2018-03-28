package org.leialearns.crystallize.util

import java.math.BigInteger.valueOf

trait RationalIsFractional extends Fractional[Rational] {
  def compare(x: Rational, y: Rational): Int = {
    (x - y).numerator.signum
  }
  def plus(x: Rational, y: Rational): Rational = {
    val xn = toBigInt(x.numerator)
    val xd = toBigInt(x.denominator)
    val yn = toBigInt(y.numerator)
    val yd = toBigInt(y.denominator)
    val n = xn * yd + yn * xd
    val d = xd * yd
    val g = BigInt(n.bigInteger.gcd(d.bigInteger))
    new Rational((n / g).bigInteger.longValue(), (d / g).bigInteger.longValue())
  }
  def negate(x: Rational): Rational = {
    new Rational(-x.numerator, x.denominator)
  }
  def minus(x: Rational, y: Rational): Rational = {
    plus(x, negate(y))
  }
  def times(x: Rational, y: Rational): Rational = {
    val xn = toBigInt(x.numerator)
    val xd = toBigInt(x.denominator)
    val yn = toBigInt(y.numerator)
    val yd = toBigInt(y.denominator)
    val n = xn * yn
    val d = xd * yd
    val g = BigInt(n.bigInteger.gcd(d.bigInteger))
    new Rational((n / g).bigInteger.longValue(), (d / g).bigInteger.longValue())
  }
  def div(x: Rational, y: Rational): Rational = {
    times(x, invert(y))
  }
  def rem(x: Rational, y: Rational): Rational = {
    throw new UnsupportedOperationException("Remainder is undefined on Rationals")
  }
  def fromInt(x: Int): Rational = {
    new Rational(x, 1)
  }
  def toLong(x: Rational): Long = {
    val xn = toBigInt(x.numerator)
    val xd = toBigInt(x.denominator)
    (xn / xd).longValue()
  }
  def toInt(x: Rational): Int = {
    toLong(x).intValue
  }
  def toDouble(x: Rational): Double = {
    x.numerator.toDouble / x.denominator
  }
  def toFloat(x: Rational): Float = {
    toDouble(x).toFloat
  }
  def invert(x: Rational): Rational = {
    new Rational(x.denominator, x.numerator)
  }
  protected def toBigInt(x: Long): BigInt = {
    new BigInt(valueOf(x))
  }
}
