package org.leialearns.crystalize

import java.util.NoSuchElementException

import org.leialearns.crystalize.item.{Node, Item, Category}
import org.leialearns.crystalize.model.{ItemCounts, Observed, Extensible}
import org.scalatest.{Matchers, FunSuite}
import java.lang.Long

import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class TestCrystal extends FunSuite with ScalaFutures with Matchers with LoggingConfiguration {
  val logger = getLogger(classOf[TestCrystal])

  test("Crystal") {
    logger.info("\n\nTest Crystal")
    val actions = Category.getCategory("action")
    val left = Item.getItem(actions, "left")
    val right = Item.getItem(actions, "right")
    val justLeftNode = Node.getNode(left)
    val justLeft = new AssignedLocation(justLeftNode, classOf[String])
    val leftRightNode = Node.getNode(justLeftNode, right)
    val leftRight = new AssignedLocation(leftRightNode, classOf[Long])
    val t0 = Crystal.head.get()
    expectNoValue(t0.get(justLeft))
    val t1 = Crystal.put(justLeft, "hi")
    val t1a = t1.remove(leftRight)
    expectNoValue(t0.get(justLeft))
    expectValue("hi", t1.get(justLeft))
    expectValue("hi", t1a.get(justLeft))
    expectNoValue(t0.get(leftRight))
    expectNoValue(t1.get(leftRight))
    expectNoValue(t1a.get(leftRight))
    val t2 = Crystal.put(leftRight, Long.valueOf(100))
    assert(!Crystal.advance(t1a))
    expectNoValue(t0.get(justLeft))
    expectValue("hi", t1.get(justLeft))
    expectValue("hi", t2.get(justLeft))
    expectNoValue(t0.get(leftRight))
    expectNoValue(t1.get(leftRight))
    expectNoValue(t1a.get(leftRight))
    expectValue(100l, t2.get(leftRight))
  }

  test("Derived") {
    logger.info("\n\nTest Derived properties")
    val actions = Category.getCategory("action")
    val left = Item.getItem(actions, "left")
    val right = Item.getItem(actions, "right")
    val justLeftNode = Node.getNode(left)
    val justLeft = new AssignedLocation(justLeftNode, classOf[String])
    val leftRightNode = Node.getNode(justLeftNode, right)
    val leftRight = new AssignedLocation(leftRightNode, classOf[Long])
    val justLeftExtensible = Extensible.createExtensibleLocation(justLeftNode)
    val t0 = Crystal.head.get()
    logger.debug(s"Time t0: ${t0.ordinal}")
    expectNoValue(t0.get(justLeftExtensible))
    val justLeftObserved = Observed.createObservedLocation(justLeftNode)
    whenReady(Crystal.update(justLeftObserved, new ItemCounts(), (counts: ItemCounts) => counts.increment(left, 9l))) {
      state => state shouldBe a [State[_]]
    }
    val t1 = Crystal.head.get()
    logger.debug(s"Time t1: ${t1.ordinal}")
    expectNoValue(t1.get(justLeftExtensible))
    whenReady(Crystal.update(justLeftObserved, new ItemCounts(), (counts: ItemCounts) => counts.increment(left, 9l))) {
      state => state shouldBe a [State[_]]
    }
    val t2 = Crystal.head.get()
    logger.debug(s"Time t2: ${t2.ordinal}")
    expectValue((), t2.get(justLeftExtensible))
  }

  def expectValue[T](expected: T, future: Future[T]): Unit = {
    whenReady(future) {result => result should equal (expected) }
  }

  def expectNoValue(future: Future[_]): Unit = {
    whenReady(future.failed) { exception => exception shouldBe a [NoSuchElementException] }
  }
}
