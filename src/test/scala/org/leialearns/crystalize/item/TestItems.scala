package org.leialearns.crystalize.item

import grizzled.slf4j.Logging
import org.leialearns.crystalize.util.LoggingConfiguration
import org.scalatest.FunSuite

class TestItems extends FunSuite with LoggingConfiguration with Logging {

  test("Items") {
    info("\n\nTest items")
    val actions = Category.getCategory("actions")
    assert(actions eq Category.getCategory("actions"))
    val responses = Category.getCategory("responses")
    assert(actions != responses)
    val dark = Item.getItem(responses, "dark")
    assert(dark eq Item.getItem(responses, "dark"))
    val light = Item.getItem(responses, "light")
    assert(dark != light)
    val left = Item.getItem(actions, "left")
    assert(left eq Item.getItem(actions, "left"))
    val right = Item.getItem(responses, "right")
    assert(left != right)
    val justLeft = Node.getNode(left)
    assert(justLeft eq Node.getNode(left))
    val leftDark = Node.getNode(justLeft, dark)
    assert(leftDark eq Node.getNode(justLeft, dark))
  }
}
