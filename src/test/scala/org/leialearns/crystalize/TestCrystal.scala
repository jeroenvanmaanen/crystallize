package org.leialearns.crystalize

import org.leialearns.crystalize.item.{Node, Item, Category}
import org.scalatest.FunSuite
import java.lang.Long

class TestCrystal extends FunSuite {
  test("Crystal") {
    val actions = Category.getCategory("action")
    val left = Item.getItem(actions, "left")
    val right = Item.getItem(actions, "right")
    val justLeftNode = Node.getNode(left)
    val justLeft = new Location(justLeftNode, classOf[String])
    val leftRightNode = Node.getNode(justLeftNode, right)
    val leftRight = new Location(leftRightNode, classOf[Long])
    val time = Crystal.getLast
    val t0 = Crystal.head
    assert(None == t0.get(justLeft))
    val t1 = Crystal.put(justLeft, "hi")
    val t1a = t1.remove(leftRight)
    assert(None == Crystal.get(justLeft, t0))
    assert(Some("hi") == Crystal.get(justLeft, t1))
    assert(Some("hi") == Crystal.get(justLeft, t1a))
    assert(None == Crystal.get(leftRight, t0))
    assert(None == Crystal.get(leftRight, t1))
    assert(None == Crystal.get(leftRight, t1a))
    val t2 = Crystal.put(leftRight, Long.valueOf(100))
    assert(None == Crystal.get(justLeft, t0))
    assert(Some("hi") == Crystal.get(justLeft, t1))
    assert(Some("hi") == Crystal.get(justLeft, t2))
    assert(None == Crystal.get(leftRight, t0))
    assert(None == Crystal.get(leftRight, t1))
    assert(None == Crystal.get(leftRight, t1a))
    assert(Some(100l) == Crystal.get(leftRight, t2))
  }
}
