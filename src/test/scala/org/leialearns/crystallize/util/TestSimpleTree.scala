package org.leialearns.crystallize.util

import org.scalatest.FunSuite

import grizzled.slf4j.Logging
import org.leialearns.crystallize.util.Rational._

//noinspection NameBooleanParameters
class TestSimpleTree extends FunSuite with LoggingConfiguration with Logging {
  private val testLimit = 300

  test("SimpleTree") {
    def comparator(value: OrderedRational, item: Rational): Int = {
      RationalIsFractional.compare(value.r, item)
    }
    def withinBounds(limit: Int)(value: OrderedRational): Boolean = value.limit < limit

    val tree = Oracle.oracle
    val unity = Rational(1, 1)
    val half = Rational(1, 2)
    var r = Rational(0, 1)
    while (r < unity) {
      debug(msg = s"Rational: ${r}")
      val value = tree.next(r, comparator, withinBounds(testLimit)).get
      assert(value.limit < testLimit)
      if (r.numerator > 0) {
        val back = tree.previous(value.r, comparator, withinBounds(testLimit)).get
        assert(back.r == r)
        val v: Rational = value.r
        val m = (value.r + r) * half
        val bracket = tree.getBracket(m, comparator, withinBounds(testLimit))
        debug(msg = s"Bracket: ${m}: ${bracket}")
        bracket match {
          case (Some(left), Some(right)) => {
            assert(left.r == r)
            assert(right.r == v)
          }
          case _ => assert(false)
        }
      }
      r = value.r
    }
  }
}
