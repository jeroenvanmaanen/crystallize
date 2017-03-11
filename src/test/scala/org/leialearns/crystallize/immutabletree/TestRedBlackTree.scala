package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{FunSuite, Matchers}

class TestRedBlackTree extends FunSuite with TreeTestTrait with Matchers with LoggingConfiguration {

  test("swap") {
    val builder = new RedBlackTreeBuilder[String,String,Unit](new SetItemKind[String] {}) {
      override def itemTokenLength(specification: String): Int = 1
      override def createItem(itemToken: String): String = itemToken
    }
    testSwap(builder, "rbv-C-Bv-D-Av-E-", "rv-C-Bvv-D-Av-E-")
    testSwap(builder, s"Rbv-C-Bv-D-${bs(1)}v-E-", s"rv-C-BVv-D-${bs(1)}v-E-")
    testSwap(builder, s"rBv-C-${bs(4)}v-D-Av-E-", s"Rv-C-${bs(4)}vv-D-Av-E-")
    testSwap(builder, s"RBv-C-${bs(4)}v-D-${bs(1)}v-E-", s"Rv-C-${bs(4)}Vv-D-${bs(1)}v-E-")
  }

  def testSwap(tree: RedBlackTreeBuilder[String,_,_], specification: String, swappedSpecification: String) = {
    val straight = tree.createTree(specification)._1
    info(s"Straight: ${tree.dump(straight)}")
    val swapped = tree.empty.swap(straight.get, LeftTreeSide, Red, BucketKind)
    info(s"Swapped:  ${tree.dump(swapped)}")
    val expected = tree.createTree(swappedSpecification)._1
    info(s"Expected: ${tree.dump(expected)}")
    assert(compareNodes(Some(swapped), expected))
  }

  test("insert") {
    val itemKind = new MapItemKind[Int, String] {
      override def compare(one: Int, other: Int) = (one / 10) compareTo (other / 10)
    }
    val builder = new RedBlackTreeBuilder[(Int, String),Int,String](itemKind) {
      val tokenRegex = "^[(]([^|]*)[|]([^)]*)[)]".r
      override def itemTokenLength(specification: String): Int = (tokenRegex.findFirstMatchIn(specification) map { case m => m.end(0) }).getOrElse(0)
      override def createItem(itemToken: String): (Int,String) = {
        val matchOption = tokenRegex.findFirstMatchIn(itemToken)
        matchOption match {
          case Some(m) =>
            (m.group(1).toInt, m.group(2))
          case _ =>
            (0, itemToken)
        }
      }
    }

    testInsert(builder, "-", (1, "one"), "b-(1|one)-")
    testInsert(builder, "b-(1|one)-", (2, "two"), "vv-(1|one)-(2|two)-")
    testInsert(builder, "vv-(1|one)-(2|two)-", (3, "three"), "vvv-(1|one)-(2|two)-(3|three)-")
    testInsert(builder, "vvv-(1|one)-(2|two)-(3|three)-", (10, "ten"), "B-vvv-(1|one)-(2|two)-(3|three)-v-(10|ten)-")
    testInsert(builder, "B-vvv-(1|one)-(2|two)-(3|three)-v-(10|ten)-", (20, "twenty"), "B-vvv-(1|one)-(2|two)-(3|three)-r-(10|ten)v-(20|twenty)-")
    testInsert(builder, "B-vvv-(1|one)-(2|two)-(3|three)-r-(10|ten)v-(20|twenty)-", (30, "thirty"), "bvvv-(1|one)-(2|two)-(3|three)-(10|ten)r-(20|twenty)v-(30|thirty)-")
    testInsert(builder, "bvvv-(1|one)-(2|two)-(3|three)-(10|ten)r-(20|twenty)v-(30|thirty)-", (100, "ratoes"), "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(30|thirty)v-(100|ratoes)-")
  }

  def testInsert(treeBuilder: RedBlackTreeBuilder[(Int, String), Int, String], specification: String, item: (Int, String), expectedSpecification: String) = {
    val before = treeBuilder.createTree(specification)._1
    info(s"Before:   ${treeBuilder.dump(before)}")
    val tree = new RedBlackTree[(Int,String), Int, String](before, treeBuilder.empty.getItemKind)
    val newTree = tree.insert(item)
    val after = newTree.getRoot
    info(s"After:    ${treeBuilder.dump(after)}")
    val expected = treeBuilder.createTree(expectedSpecification)._1
    info(s"Expected: ${treeBuilder.dump(expected)}")
    assert(compareNodes(after, expected))
  }

  def bs(n: Int): String = {
    s"V-v-${n}v-${n+1}--"
    // s"V(-|v(-|$n|v(-|${n+1}|-))|-)"
  }

  def compareNodes[A](node: Option[RedBlackNode[A]], other: Option[RedBlackNode[A]]): Boolean = {
    (node, other) match {
      case (None, None) => true
      case (Some(theNode), Some(theOther)) =>
        compareBranches(theNode.getLeftNode, theOther.getLeftNode) &&
        compareBranches(Some(theNode.getMiddle), Some(theOther.getMiddle)) &&
        compareBranches(theNode.getRightNode, theOther.getRightNode) &&
        theNode.getNodeKind == theOther.getNodeKind
      case _ => false
    }
  }

  def compareBranches[A](node: Option[Either[A,TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]]], other: Option[Either[A,TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]]]): Boolean = {
    (node, other) match {
      case (None, None) => true
      case (Some(Left(theNodeItem)), Some(Left(theOtherItem))) => theNodeItem == theOtherItem
      case (Some(Right(theNode)), Some(Right(theOther))) =>
        compareNodes(Some(theNode), Some(theOther))
      case _ => false
    }
  }

  test("Red black tree") {
    val empty = new RedBlackTree[String, String, Unit](None, new SetItemKind[String] {})
    testTree(empty, Red, Black)
  }

  test("Red black tree map") {
    val itemKind = new MapItemKind[Int, String] {
      override def compare(one: Int, other: Int) = (one / 10) compareTo (other / 10)
    }
    val empty = new RedBlackTree[(Int, String), Int, String](None, itemKind)
    List(
      (1, "one"),
      (2, "two"),
      (3, "three"),
      (10, "ten"),
      (20, "twenty"),
      (30, "thirty"),
      (100, "ratoes")
    ).foldLeft(empty) ((tree, triple) => {
      triple match {
        case (number, label) => insertFunction(tree, number, label)
      }
    })
  }

  def insertFunction(tree: RedBlackTree[(Int, String), Int, String], number: Int, label: String): RedBlackTree[(Int, String), Int, String] = {
    val result = tree.insert((number, label))
    info(s"Inserted: $number: $label: ${result.dump}")
    result.verifyBalance()
    result
  }
}
