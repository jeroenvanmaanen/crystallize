package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{FunSuite, Matchers}

class TestSimpleTree extends FunSuite with TreeTestTrait with Matchers with LoggingConfiguration {

  test("Simple tree") {
    val empty = new SimpleTree[String, String, Unit](None, new SetItemKind[String]{})
    testTree(empty, (), ())
  }

  test("Simple tree map") {
    val itemKind = new MapItemKind[Int, String]{
      override def compare(one: Int, other: Int) = (one / 10) compareTo (other / 10)
    }
    val empty = new SimpleTree[(Int, String), Int, String](None, itemKind)

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
