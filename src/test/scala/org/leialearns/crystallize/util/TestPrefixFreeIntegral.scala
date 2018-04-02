package org.leialearns.crystallize.util

import java.io.{IOException, StringReader}

import grizzled.slf4j.Logging
import org.scalatest.FunSuite

import scala.util.control.Breaks

class TestPrefixFreeIntegral extends FunSuite with Logging {

  def singlePrefixEncode[T](n: T)(implicit prefixFree: PrefixFreeIntegral[T]) {
    val encoded = prefixFree.prefixEncode(n)
    logger.info(s"Prefix-free: $n: [$encoded]")
    val encodedReader = new StringReader(encoded)
    val decoded = prefixFree.prefixDecode(encodedReader)
    intercept[IOException] {
      prefixFree.readBit(encodedReader)
    }
    assert(n == decoded)

    val bitReader = new StringReader(encoded)
    val loop = new Breaks
    var i = 0
    loop.breakable {
      while (true) {
        try {
          prefixFree.readBit(bitReader)
          i += 1
        } catch {
          case _: IOException =>
            loop.break()
        }
      }
    }
    val length = prefixFree.descriptionLength(n)
    assert(i == length)
  }

  test("Composition of prefixDecode and prefixEncode should be the identity function") {
    for (i <- 0 to 40) {
      singlePrefixEncode(i)(IntIsPrefixFree)
    }
    for (i <- 1 to 16) {
      val n = BigInt.int2bigInt(2).pow(i) - 1
      val m = n - 1
      singlePrefixEncode(m)(BigIntIsPrefixFree)
      singlePrefixEncode(n)(BigIntIsPrefixFree)
    }
  }

  implicit object IntIsPrefixFree extends PrefixFreeIntegral[Int]
  implicit object BigIntIsPrefixFree extends PrefixFreeIntegral[BigInt]
}
