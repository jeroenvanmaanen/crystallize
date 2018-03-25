package org.leialearns.crystallize.expectations

import org.scalatest.FunSuite
import org.leialearns.crystallize.item._
import org.leialearns.crystallize.model.ItemCounts
import org.leialearns.crystallize.util.LoggingConfiguration
import org.leialearns.crystallize.util.Rational._
import grizzled.slf4j.Logging
import org.scalactic.source.Position.apply
import org.leialearns.crystallize.util.Dump
import org.leialearns.crystallize.util.Oracle

class TestItemCountsOptimizer extends FunSuite with LoggingConfiguration with Logging {
  test("ItemCountsOptimizer") {
    val color = Category("color")
    val red = Item(color, "red")
    val green = Item(color, "green")
    val blue = Item(color, "blue")
    val map = List((red, 306L), (green, 599L), (blue, 95L)).toMap
    val total = map.foldLeft(0L)(_ + _._2)
    val itemCounts = new ItemCounts(map, total)
    Dump.dump("Item counts", itemCounts.map).foreach(debug(_))

    val optimized = ItemCountsOptimizer.optimize(itemCounts)
    Dump.dump("Optimized", optimized).foreach(debug(_))
    optimized.toStream.map(e => assert(e._2._1 > ZERO))
    val sum = optimized.foldLeft(ZERO)(_ + _._2._1)
    assert(sum == ONE)

    val bounds = optimized.toSeq.map(e => (e._1, e._2._2)).map(e => (e._1, Oracle.lookup.get(e._2).get))
    val boundsMap = Map.empty ++ bounds
    Dump.dump("Bounds map", boundsMap).foreach(debug(_))
    val redBound = boundsMap.get(red).get
    val greenBound = boundsMap.get(green).get
    val blueBound = boundsMap.get(blue).get
    assert(greenBound.ordinal == 0)
    assert(greenBound.r == ONE)
    assert(greenBound.r - redBound.r == optimized.get(green).get._1)
    assert(redBound.r - blueBound.r == optimized.get(red).get._1)
    assert(blueBound.r == optimized.get(blue).get._1)
  }
}