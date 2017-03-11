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
    val straight = tree.createTree(specification)._1.get
    info(s"Straight: ${tree.dump(straight)}")
    val swapped = tree.empty.swap(straight, LeftTreeSide, Red, BucketKind)
    info(s"Swapped:  ${tree.dump(swapped)}")
    val expected = tree.createTree(swappedSpecification)._1.get
    info(s"Expected: ${tree.dump(swapped)}")
    assert(compareNodes(Some(swapped), Some(expected)))
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
