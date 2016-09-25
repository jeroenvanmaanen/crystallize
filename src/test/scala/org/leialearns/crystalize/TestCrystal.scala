package org.leialearns.crystalize

import org.leialearns.crystalize.item.{Node, Item, Category}
import org.scalatest.FunSuite

class TestCrystal extends FunSuite {
  test("Crystal") {
    val actions = Category.getCategory("action")
    val left = Item.getItem(actions, "left")
    val right = Item.getItem(actions, "right")
    val justLeftNode = Node.getNode(left)
    val justLeft = new Location(justLeftNode, classOf[String])
    val leftRightNode = Node.getNode(justLeftNode, right)
    val leftRight = new Location[Long](leftRightNode, classOf[Long])
    Crystal.put(justLeft, "hi")
    Crystal.put(leftRight, 100l)
  }
}
