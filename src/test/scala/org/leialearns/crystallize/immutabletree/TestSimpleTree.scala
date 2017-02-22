package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.simple.Simple
import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{Matchers, FunSuite}

class TestSimpleTree extends FunSuite with Matchers with LoggingConfiguration {
  def testCreateNode(tree: SimpleTree[String, String], leftNodeOption: Option[Simple[String]], item: String, rightNodeOption: Option[Simple[String]]): Simple[String] = {
    val result = testCreateNode(tree, leftNodeOption, Left(item), rightNodeOption)
    assert(result.untwist.getItem == item)
    result
  }
  def testCreateNode(tree: SimpleTree[String, String], leftNodeOption: Option[Simple[String]], bucket: Simple[String], rightNodeOption: Option[Simple[String]]): Simple[String] = {
    val result = testCreateNode(tree, leftNodeOption, Right(bucket), rightNodeOption)
    result
  }
  def testCreateNode(tree: SimpleTree[String, String], leftNodeOption: Option[Simple[String]], middle: Either[String,Simple[String]], rightNodeOption: Option[Simple[String]]): Simple[String] = {
    val result = tree.createNode(leftNodeOption, middle, rightNodeOption, ())
    if (leftNodeOption.isEmpty && middle.isRight && rightNodeOption.isEmpty) {
      assert(result == middle.right.get)
    } else {
      val untwisted = result.untwist
      val untwistedLeftNode = untwisted.getLeftNode map ((either) => tree.asTree(either))
      val untwistedRightNode = untwisted.getRightNode map ((either) => tree.asTree(either))
      assert(untwistedLeftNode == leftNodeOption)
      assert(untwistedRightNode == rightNodeOption)
      assert(untwisted.getMiddle == middle)
      middle match {
        case Left(middleItem) =>
          assert(untwisted.getItem == middleItem)
          assert(untwisted.getBucket.isEmpty)
        case Right(middleBucket) =>
          assert(untwisted.getItem == middleBucket.getItem)
          assert(untwisted.getBucket == Some(middleBucket))
      }
    }
    result
  }

  test("Simple tree") {
    val empty = new SimpleTree[String, String](None, new SetKeyExtractor[String], new SetKeyKind[String]{})

    // Item nodes
    val n1 = testCreateNode(empty, None, "one", None)
    val n2 = testCreateNode(empty, None, "two", None)
    val n3 = testCreateNode(empty, Some(n1), "three", None)
    val n4 = testCreateNode(empty, None, "four", Some(n2))
    val n5 = testCreateNode(empty, Some(n1), "five", Some(n2))
    val n6 = testCreateNode(empty, Some(n3), "five", None)
    val n7 = testCreateNode(empty, None, "five", Some(n4))
    val n8 = testCreateNode(empty, Some(n3), "five", Some(n4))

    // Bucket nodes
    val n11 = testCreateNode(empty, None, n8, None)
    val n12 = testCreateNode(empty, None, n8, None)
    val n13 = testCreateNode(empty, Some(n1), n8, None)
    val n14 = testCreateNode(empty, None, n8, Some(n2))
    val n15 = testCreateNode(empty, Some(n1), n8, Some(n2))
    val n16 = testCreateNode(empty, Some(n3), n8, None)
    val n17 = testCreateNode(empty, None, n8, Some(n4))
    val n18 = testCreateNode(empty, Some(n3), n8, Some(n4))

    val n23 = testCreateNode(empty, Some(n11), n8, None)
    val n24 = testCreateNode(empty, None, n8, Some(n12))
    val n25 = testCreateNode(empty, Some(n11), n8, Some(n2))
    val n35 = testCreateNode(empty, Some(n1), n8, Some(n12))


    val one = empty.insert("one")
    assert(Some("one") == one.find("one"))
    assert(None == one.find("two"))

    val two = one.insert("two")
    assert(Some("one") == two.find("one"))
    assert(Some("two") == two.find("two"))
    assert(None == two.find("three"))
  }

  test("Simple tree map") {
    val empty = new SimpleTree[(Int, _), Int](None, new MapKeyExtractor[Int], new MapKeyKind[Int]{ override def getKeyHashCode(key: Int) = key / 10 })

    val tree = empty
      .insert((50, "one"))
      .insert((20, "two"))
      .insert((55, "one b"))
      .insert((10, "three"))
      .insert((30, "four"))
      .insert((40, "five"))
      .insert((11, "three b"))
      .insert((70, "six"))
      .insert((60, "seven"))
      .insert((59, "eight"))
    val treeDump = tree.dump
    info(s"Tree dump: $treeDump")
    val leftDump = "<n><n><n/><i>(10,three)</i><n>(11,three b)</n></n><i>(20,two)</i><n><n/><i>(30,four)</i><n>(40,five)</n></n></n>"
    val rightDump = "<n><n>(60,seven)</n><i>(70,six)</i><n/></n>"
    assert(treeDump == s"<t><n>$leftDump<i><n><n/><i><n><n/><i>(50,one)</i><n>(55,one b)</n></n></i><n>(59,eight)</n></n></i>$rightDump</n></t>")
    assert(Some("three") == (tree.find(10) map (_._2)))
    assert(Some("three b") == (tree.find(11) map (_._2)))
    assert(Some("two") == (tree.find(20) map (_._2)))
    assert(Some("four") == (tree.find(30) map (_._2)))
    assert(Some("five") == (tree.find(40) map (_._2)))
    assert(Some("one") == (tree.find(50) map (_._2)))
    assert(Some("one b") == (tree.find(55) map (_._2)))
    assert(Some("eight") == (tree.find(59) map (_._2)))
    assert(Some("seven") == (tree.find(60) map (_._2)))
    assert(Some("six") == (tree.find(70) map (_._2)))

    val it = tree.iterator
    assert((10, "three") == it.next())
    assert((11, "three b") == it.next())
    assert((20, "two") == it.next())
    assert((30, "four") == it.next())
    assert((40, "five") == it.next())
    assert((50, "one") == it.next())
    assert((55, "one b") == it.next())
    assert((59, "eight") == it.next())
    assert((60, "seven") == it.next())
    assert((70, "six") == it.next())

    val it2 = tree.iterator
    assert((10, "three") == it2.next())
    assert(it2.hasNext)
    assert((11, "three b") == it2.next())
    assert(it2.hasNext)
    assert((20, "two") == it2.next())
    assert(it2.hasNext)
    assert((30, "four") == it2.next())
    assert(it2.hasNext)
    assert((40, "five") == it2.next())
    assert(it2.hasNext)
    assert((50, "one") == it2.next())
    assert(it2.hasNext)
    assert((55, "one b") == it2.next())
    assert(it2.hasNext)
    assert((59, "eight") == it2.next())
    assert(it2.hasNext)
    assert((60, "seven") == it2.next())
    assert(it2.hasNext)
    assert((70, "six") == it2.next())
    assert(!it2.hasNext)
  }
}
