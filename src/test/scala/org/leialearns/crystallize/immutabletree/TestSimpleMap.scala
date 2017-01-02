package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{Matchers, FunSuite}

class TestSimpleMap extends FunSuite with Matchers with LoggingConfiguration {

  test("Simple map") {
    val empty = new SimpleMap[(String, Int), Int]()
    val single = empty + ((("M", 5), 1))
    assert(Some(1) == single.get(("M", 5)))
    val it = single.iterator
    assert((("M", 5), 1) == it.next)
    assert(!it.hasNext)
  }

  test("Test a lot of trees") {
    info ("A")
    val testKeyKind = new KeyKind[(String,Int)] {
      override def compare(one: (String, Int), other: (String, Int)): Int = {
        one._1 compareTo other._1
      }
      override def equals(one: (String,Int), other: (String,Int)): Boolean = {
        one == other
      }
    }
    info ("B")
    val emptyMap = new SimpleMap[(String,Int),Int](testKeyKind)
    info ("C")

    var n = BigInt.int2bigInt(0)
    val depth = 2
    while (n.bitLength < 2 * 7) {
      info(s"Test $n")
      testNode(emptyMap, "", n) match {
        case (treeMap, remainder, bits) =>
          info(treeMap.dump)
          if (n == BigInt.int2bigInt(0)) {
            n = BigInt.int2bigInt(1) << (7 * (depth - 1))
          } else {
            val shift = ((n.bitLength + 5 - bits) / 6) * 6
            n += BigInt.int2bigInt(1) << shift
          }
      }
    }
  }

  def testNode(tree: SimpleMap[(String,Int),Int], prefix: String, n: BigInt): (SimpleMap[(String,Int),Int], BigInt, Int) = {
    info(s"Test node: $prefix: $n")
    val shift = (n.bitLength / 7) * 7
    val byte = (if (n > 0) n >> shift else n).byteValue()
    val frame = BigInt.int2bigInt(0x7F)
    var remainder = n & ~(frame << shift)
    var bits = 7
    val hasLeft = (byte & 0x40) != 0
    val hasRight = (byte & 0x20) != 0
    val (beforeMiddle, afterMiddle) = getCounts(byte & 0x1F)
    var newTree = tree
    if (beforeMiddle > 0) {
      for (i <- 1 to beforeMiddle) { newTree = newTree + (((prefix + "M", i), 5)) }
    }
    newTree = newTree + (((prefix + "M", 7), 5))
    if (afterMiddle > 0) {
      for (i <- 0 to afterMiddle) { newTree = newTree + (((prefix + "M", i + 7), 5)) }
    }
    if (hasLeft) {
      testNode(newTree, prefix + "L", remainder) match {
        case (subTree, subRemainder, subBits) =>
          newTree = subTree
          remainder = subRemainder
          bits += subBits
      }
    }
    if (hasRight) {
      testNode(newTree, prefix + "R", remainder) match {
        case (subTree, subRemainder, subBits) =>
          newTree = subTree
          remainder = subRemainder
          bits += subBits
      }
    }
    (newTree, remainder, bits)
  }

  def getCounts(code: Int): (Int,Int) = {
    if (code < 25) {
      (code / 5, code % 5)
    } else if (code <= 28) {
      (5, code - 23)
    } else {
      (code - 26, 5)
    }
  }
}
