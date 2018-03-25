package org.leialearns.crystallize.util

import org.scalatest.FunSuite

import grizzled.slf4j.Logging
import org.leialearns.crystallize.util.Rational._
import scala.math._
import scala.math.Numeric

class TestSimpleTree extends FunSuite with LoggingConfiguration with Logging {

  test("SimpleTree") {
    def comparator(value: OrderedRational, item: Rational): Int = {
      RationalIsFractional.compare(value.r, item)
    }
    def withinBounds(limit: Int)(value: OrderedRational) = value.limit < limit

    val tree = Oracle.oracle
    val unity = Rational(1, 1)
    val half = Rational(1, 2)
    var r = Rational(0, 1)
    while (r < unity) {
      debug(s"Rational: ${r}")
      val value = tree.next(r, comparator, withinBounds(300)).get
      assert(value.limit < 300)
      if (r.numerator > 0) {
        val back = tree.previous(value.r, comparator, withinBounds(300)).get
        assert(back.r == r)
        val v: Rational = value.r
        val m = (value.r + r) * half
        val bracket = tree.getBracket(m, comparator, withinBounds(300))
        debug(s"Bracket: ${m}: ${bracket}")
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
