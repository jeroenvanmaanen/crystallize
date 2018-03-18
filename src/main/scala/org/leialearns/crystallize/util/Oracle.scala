package org.leialearns.crystallize.util

import java.io.InputStreamReader
import java.io.BufferedReader
import scala.io.Source
import java.lang.invoke.MethodHandles
import grizzled.slf4j.Logging

object Oracle extends Logging {
  val oracle = new SimpleTree(new OrderedRational(new Rational(1,1), 0, 0))
  getPrecomputed("leia-oracle.data")

  def getPrecomputed(resourcePath: String) {
    val specRe = "^[(]([0-9]*),([0-9]*) % ([0-9]*)[)]$".r
    info("Get precomputed ordered rationals")
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    var i: Long = 0L
    val source = Source.fromInputStream(stream)
    for(line <- source.getLines()) {
      i += 1
      trace(s"Line: ${line}")
      if (!line.startsWith("--")) {
        line match {
          case specRe(limit, numerator, denominator) =>
            oracle.add(new OrderedRational(new Rational(numerator.toLong ,denominator.toLong), limit.toLong, i))
        }
      }
    }
    if (logger.isTraceEnabled) {
      for (line <- Dump.dump("", oracle)) {
        trace(line)
      }
    }
  }
}