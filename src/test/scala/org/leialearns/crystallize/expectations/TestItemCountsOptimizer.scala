package org.leialearns.crystallize.expectations

import grizzled.slf4j.Logging
import org.leialearns.crystallize.item._
import org.leialearns.crystallize.model.ItemCounts
import org.leialearns.crystallize.util.{Dump, LoggingConfiguration, Oracle, Rational}
import org.leialearns.crystallize.util.Rational._
import org.scalatest.FunSuite

class TestItemCountsOptimizer extends FunSuite with LoggingConfiguration with Logging {
  val color = Category("color")
  val red = Item(color, "red")
  val green = Item(color, "green")
  val blue = Item(color, "blue")

  test("ItemCountsOptimizer") {
    testMap(List((red, 306L), (green, 599L), (blue, 95L)).toMap)
    testMap(List((red, 2L), (green, 1000000L), (blue, 1L)).toMap)
  }

  test("Uniform") {
    val number = Category("number")
    val map = (for (i <- 1 to 100) yield (Item(number, s"#${i}"), 1L)).toMap
    val optimized = optimize(map)
    optimized.values.foreach {
      case (r, n) =>
        assert(r == Rational(1, 100))
        assert(n == 0L)
    }
  }

  def testMap(map: Map[Item,Long]): Unit = {
    val optimized = optimize(map)
    optimized.toStream.map(e => assert(e._2._1 > ZERO))
    val sum = optimized.foldLeft(ZERO)(_ + _._2._1)
    assert(sum == ONE)
    assert(optimized(red)._1 > ZERO)
    assert(optimized(green)._1 > ZERO)
    assert(optimized(blue)._1 > ZERO)

    val bounds = optimized.toSeq.map(e => (e._1, e._2._2)).map(e => (e._1, Oracle.lookup(e._2)))
    val boundsMap = Map.empty ++ bounds
    Dump.dump("Bounds map", boundsMap).foreach(debug(_))
    val redBound = boundsMap(red)
    val greenBound = boundsMap(green)
    val blueBound = boundsMap(blue)
    assert(greenBound.ordinal == 0)
    assert(greenBound.r == ONE)
    assert(greenBound.r - redBound.r == optimized(green)._1)
    assert(redBound.r - blueBound.r == optimized(red)._1)
    assert(blueBound.r == optimized(blue)._1)
  }

  def optimize(map: Map[Item,Long]): Map[Item, (Rational, Long)] = {
    val total = map.foldLeft(0L)(_ + _._2)
    val itemCounts = new ItemCounts(map, total)
    Dump.dump("Item counts", itemCounts.map).foreach(debug(_))
    val optimized = ItemCountsOptimizer.optimize(itemCounts)
    Dump.dump("Optimized", optimized).foreach(debug(_))
    optimized
  }
}