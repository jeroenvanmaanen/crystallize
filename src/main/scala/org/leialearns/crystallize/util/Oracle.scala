package org.leialearns.crystallize.util

import scala.io.Source
import grizzled.slf4j.Logging

object Oracle extends Logging {
  val oracle = new SimpleTree(new OrderedRational(new Rational(1,1), 0, 0))
  var lookup: Map[Long, OrderedRational] = getPrecomputed("leia-oracle.data")

  def getBracket(item: Rational, limit: Int): (Option[OrderedRational], Option[OrderedRational]) = {
    oracle.getBracket(item, comparator, withinBounds(limit))
  }

  def comparator(value: OrderedRational, item: Rational): Int = {
    value.r.compareTo(item)
  }

  def withinBounds(limit: Long)(value: OrderedRational): Boolean = value.limit < limit

  def getPrecomputed(resourcePath: String): Map[Long, OrderedRational] = {
    val specRe = "^[(]([0-9]*),([0-9]*) % ([0-9]*)[)]$".r
    info("Get precomputed ordered rationals")
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    var i: Long = 0L
    val source = Source.fromInputStream(stream)
    var lookup: Map[Long, OrderedRational] = Map.empty
    for(line <- source.getLines()) {
      trace(s"Line: ${line}")
      if (!line.startsWith("--")) {
        line match {
          case specRe(limit, numerator, denominator) =>
            val bound = new OrderedRational(new Rational(numerator.toLong ,denominator.toLong), limit.toLong, i)
            oracle.add(bound)
            lookup = lookup + ((i, bound))
            i += 1
        }
      }
    }
    if (logger.isTraceEnabled) {
      for (line <- Dump.dump("", oracle)) {
        trace(line)
      }
    }
    lookup
  }
}