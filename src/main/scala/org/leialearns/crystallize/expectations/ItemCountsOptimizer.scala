package org.leialearns.crystallize.expectations

import org.leialearns.crystallize.model.ItemCounts
import org.leialearns.crystallize.util.Dump
import org.leialearns.crystallize.util.Oracle
import org.leialearns.crystallize.util.OrderedRational
import org.leialearns.crystallize.util.Rational
import org.leialearns.crystallize.util.Rational._
import grizzled.slf4j.Logging
import org.leialearns.crystallize.item.Item

object ItemCountsOptimizer extends Logging {
  def optimize(itemCounts: ItemCounts): Map[Item,(Rational,Long)] = {
    val (accumulated, total) = accumulate(itemCounts.map.toSeq)
    if (isTraceEnabled) {
      trace(s"Total: ${total}")
      Dump.dump("Accumulated", accumulated).foreach(trace(_))
    }
    val measured = accumulated.map(e => (e._1, Rational(e._2, total)))
    if (isTraceEnabled) {
      Dump.dump("Measured", measured).foreach(trace(_))
    }
    val brackets = measured.map(e => (e, Oracle.getBracket(e._2, total.toInt)))
    if (isTraceEnabled) {
      Dump.dump("Brackets", brackets).foreach(trace(_))
    }
    val approximated = brackets.map(b => (b._1._1, closest(b._1._2, b._2)))
    if (isTraceEnabled) {
      Dump.dump("Approximated", approximated).foreach(trace(_))
    }
    val distinct = spread(approximated, total)
    if (isTraceEnabled) {
      Dump.dump("Distinct", distinct).foreach(trace(_))
    }
    val result: Map[Item,(Rational,Long)] = distinct.map(differences).getOrElse(uniform(itemCounts.map.keySet)).toMap
    if (isTraceEnabled) {
      Dump.dump("Result", result).foreach(trace(_))
    }
    result
  }

  def accumulate[K](entries: Seq[(K,Long)]): (List[(K,Long)], Long) = {
    val sorted = entries.sortWith((a, b) => a._2 < b._2)
    sorted.foldLeft((Nil: List[(K,Long)], 0L)) {
      case ((accumulated, total), (key, value)) => {
        trace(s"Accumulated type: ${accumulated.getClass.getSimpleName}")
        trace(s"Entry: ${key}: ${value}")
        val sum = total + value
        trace(s"Sum: ${sum}")
        val nextAccumulated = (key, sum) :: accumulated
        if (isTraceEnabled) {
          Dump.dump("Next accumulated", nextAccumulated).foreach(trace(_))
        }
        (nextAccumulated, sum)
      }
    }
  }

  def spread[K](approximated: List[(K,OrderedRational)], limit: Long): Option[List[(K,OrderedRational)]] = {
    val oracle = Oracle.oracle
    val proceed: Rational => Option[OrderedRational] = r => oracle.next(r, Oracle.comparator, Oracle.withinBounds(limit))
    val start: Option[(List[(K,OrderedRational)],Rational)] = Some((Nil, ZERO))
    val result = approximated.foldRight(start) {
      case ((key, bound), Some((done, last))) =>
        proceed(last)
          .map(next => {
            val adjusted = if (next.r > bound.r) next else bound
            ((key, adjusted) :: done, adjusted.r)
          })
      case (_, None) => None
    }
    result.map(_._1)
  }

  def differences[K](approximated: List[(K,OrderedRational)]): List[(K,(Rational,Long))] = {
    approximated match {
      case Nil => Nil
      case head :: tail =>
        val differences = tail.foldLeft((Nil: List[(K,(Rational, Long))], head)) {
          case ((done, last), (key, bound)) => {
            ((last._1, (last._2.r - bound.r, last._2.ordinal)) :: done, (key, bound))
          }
        }
        (differences._2._1, (differences._2._2.r, differences._2._2.ordinal)) :: differences._1
    }
  }

  def uniform(keySet: Set[Item]): List[(Item,(Rational,Long))] = {
    val denominator = keySet.size.longValue
    (for (i <- keySet) yield (i, (Rational(1, denominator), 0L))).toList
  }

  def closest(r: Rational, bracket: (Option[OrderedRational], Option[OrderedRational])): OrderedRational = {
    val leftDistance = distance(r, bracket._1)
    val rightDistance = distance(r, bracket._2)
    if (leftDistance < rightDistance) {
      bracket._1.get
    } else {
      bracket._2.get
    }
  }

  def distance(r: Rational, b: Option[OrderedRational]): Rational = {
    b.map(bound => abs(r - bound.r)).getOrElse(ONE)
  }
}