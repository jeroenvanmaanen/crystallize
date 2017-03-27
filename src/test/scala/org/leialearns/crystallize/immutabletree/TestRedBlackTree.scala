package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable

class TestRedBlackTree extends FunSuite with TreeTestTrait with Matchers with LoggingConfiguration {

  test("swap") {
    val builder = new RedBlackTreeBuilder[String,String,Unit](new SetItemKind[String] {}) {
      override def itemTokenLength(specification: String): Int = 1
      override def createItem(itemToken: String): String = itemToken
      override def serializeItem(item: String): String = item.substring(0, 1)
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
    val builder = createTreeBuilder(10)

    testInsert(builder, "-", (1, "one"), "b-(1|one)-")
    testInsert(builder, "b-(1|one)-", (2, "two"), "vv-(1|one)-(2|two)-")
    testInsert(builder, "vv-(1|one)-(2|two)-", (3, "three"), "vvv-(1|one)-(2|two)-(3|three)-")
    testInsert(builder, "vvv-(1|one)-(2|two)-(3|three)-", (10, "ten"), "B-vvv-(1|one)-(2|two)-(3|three)-v-(10|ten)-")
    testInsert(builder, "B-vvv-(1|one)-(2|two)-(3|three)-v-(10|ten)-", (20, "twenty"), "B-vvv-(1|one)-(2|two)-(3|three)-r-(10|ten)v-(20|twenty)-")
    testInsert(builder, "B-vvv-(1|one)-(2|two)-(3|three)-r-(10|ten)v-(20|twenty)-", (30, "thirty"), "bvvv-(1|one)-(2|two)-(3|three)-(10|ten)r-(20|twenty)v-(30|thirty)-")
    testInsert(builder, "bvvv-(1|one)-(2|two)-(3|three)-(10|ten)r-(20|twenty)v-(30|thirty)-", (100, "ratoes"), "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(30|thirty)v-(100|ratoes)-")
    testInsert(builder, "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(30|thirty)v-(100|ratoes)-", (101, "ratoes satoe"), "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(30|thirty)vv-(100|ratoes)-(101|ratoes satoe)-")
    testInsert(builder, "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(30|thirty)vv-(100|ratoes)-(101|ratoes satoe)-", (300, "tiga ratoes"), "bbvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)b-(30|thirty)R-vv-(100|ratoes)-(101|ratoes satoe)-v-(300|tiga ratoes)-")

    testInsert(builder, "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)r-(100|ratoes)v-(300|tiga ratoes)-", (50, "fifty"), "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)rv-(50|fifty)-(100|ratoes)v-(300|tiga ratoes)-")
    testInsert(builder, "brvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)rv-(50|fifty)-(100|ratoes)v-(300|tiga ratoes)-", (70, "seventy"), "bbvvv-(1|one)-(2|two)-(3|three)-(10|ten)-(20|twenty)b-(50|fifty)rv-(70|seventy)-(100|ratoes)v-(300|tiga ratoes)-")
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

  test("equality") {
    val treeBuilder = createTreeBuilder(3)
    val itemKind = treeBuilder.getItemKind
    var currentRedBlackTree = new RedBlackTree[(Int, String),Int,String](None, itemKind)
    var currentScalaMap = new immutable.HashMap[Int,String]
    val random = scala.util.Random
    val iterations = 3000
    val bound = (20 + iterations) / 3
    for (n <- 1 to iterations) {
      val label = s"n$n"
      val x = random.nextInt(bound)
      val pair = (x, label)
      val newRedBlackTree = currentRedBlackTree.insert(pair)
      val newScalaMap = currentScalaMap + pair
      assert((newRedBlackTree.find(x) map (_._2)) == newScalaMap.get(x))

      newRedBlackTree.verifyBalance()

      val count = compareItems(newRedBlackTree, newScalaMap, itemKind)
      if (count != newScalaMap.size) {
        info(s"Old red-black tree: ${currentRedBlackTree.dump}")
        info(s"New red-black tree: ${newRedBlackTree.dump}")
      }
      assert(count == newScalaMap.size, s"Wrong size: $pair")
      currentRedBlackTree = newRedBlackTree
      currentScalaMap = newScalaMap
    }
//*
    checkRemove(treeBuilder, 11, "bBv-(3|n67})-vv-(7|n23})-(8|n50})--(11|n15})RBRvv-(14|n76})-(12|n95})-vvv-(17|n69})-(15|n89})-(16|n44})--vv-(20|n63})-(18|n66})--vvv-(25|n75})-(24|n48})-(26|n86})-Brvv-(27|n99})-(28|n85})-(32|n100})v-(34|n51})-vvv-(36|n96})-(37|n17})-(38|n57})--")

    var treeSize: Int = currentRedBlackTree.size
    while (currentRedBlackTree.getRoot.isDefined) {
      var currentNode = currentRedBlackTree.getRoot.get
      var ancestors: List[TreeNodeTrait[(Int,String), RedBlackNode[(Int,String)] with RedBlackNode[(Int,String)]]] = currentNode :: Nil
      while (currentNode.getLeftNode.isDefined || currentNode.getRightNode.isDefined) {
        val nextEither =
          if (currentNode.getLeftNode.isEmpty) {
            currentNode.getRightNode.get
          } else if (currentNode.getRightNode.isEmpty) {
            currentNode.getLeftNode.get
          } else {
            (if (random.nextBoolean()) currentNode.getLeftNode else currentNode.getRightNode).get
          }
        currentNode = currentRedBlackTree.getNodeFactory.asTree(nextEither, BucketKind)
        ancestors = currentNode :: ancestors
      }
      while (ancestors.drop(1).nonEmpty && random.nextBoolean()) {
        ancestors = ancestors.drop(1)
      }
      if (ancestors.nonEmpty) {
        val victim = ancestors(0).getItem._1
        val expectedSize = treeSize - 1
        currentRedBlackTree = checkRemove(treeBuilder, victim, expectedSize, currentRedBlackTree)
        treeSize = expectedSize
      }
    }
// */
  }

  def checkRemove(treeBuilder: RedBlackTreeBuilder[(Int,String),Int,String], victim: Int, specification: String): Option[RedBlackNode[(Int,String)]] = {
    val treeOption = treeBuilder.createTree(specification)._1
    val tree = new RedBlackTree[(Int,String),Int,String](treeOption, treeBuilder.getItemKind)
    val oldSize = tree.size
    tree.verifyBalance()
    checkRemove(treeBuilder, victim, oldSize - 1, tree)
    tree.getRoot
  }

  def checkRemove(treeBuilder: RedBlackTreeBuilder[(Int,String),Int,String], victim: Int, expectedSize: Int, tree: RedBlackTree[(Int,String),Int,String]): RedBlackTree[(Int,String),Int,String] = {
    var result: Option[RedBlackTree[(Int,String),Int,String]] = None
    try {
      info(s"Removing: $victim")
      val smallerRedBlackTree = tree.remove(victim)
      result = Some(smallerRedBlackTree)
      assert(smallerRedBlackTree.size == expectedSize)
      // smallerRedBlackTree.verifyBalance()
      for (item <- smallerRedBlackTree) {
        val key = treeBuilder.getItemKind.getKey(item)
        assert(smallerRedBlackTree.find(key).isDefined, s"Could not find: $key")
      }
      smallerRedBlackTree
    } catch {
      case exception: Throwable =>
        info(s"Exception while removing key: $victim: $exception")
        info(s"Tree specification: ${treeBuilder.serializeTree(tree.getRoot)}")
        info(s"Original tree dump: ${tree.dump}")
        info(s"Result tree dump: ${((result flatMap (_.getRoot)) map (tree.dump(_, Nil))).getOrElse("None")}")
        throw exception
    }
  }

  def createTreeBuilder(fudge: Int) = {
    val itemKind = new MapItemKind[Int, String] {
      override def compare(one: Int, other: Int) = (one / fudge) compareTo (other / fudge)
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
      override def serializeItem(item: (Int,String)): String = {
        s"(${item._1}|${item._2}})"
      }
    }
    builder
  }

  def compareItems(newRedBlackTree: RedBlackTree[(Int, String), Int, String], newScalaMap: Map[Int, String], itemKind: ItemKind[(Int, String), Int, String]): Int = {
    var count = 0
    var previousPairOption: Option[(Int, String)] = None
    for (p: (Int, String) <- newRedBlackTree) {
      count += 1
      val thisKey = p._1
      assert(newScalaMap.contains(thisKey))
      previousPairOption match {
        case Some(previousPair) =>
          getLogger(classOf[TestRedBlackTree]).trace(s"Pairs: $p: $previousPair")
          val previousKey = previousPair._1
          assert(!itemKind.equals(thisKey, previousKey), s"Same key: $previousPair: $p")
          assert(itemKind.compare(thisKey, previousKey) >= 0, s"Wrong order: $previousPair: $p")
        case _ => ()
      }
      previousPairOption = Some(p)
    }

    count
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
