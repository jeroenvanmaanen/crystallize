package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.util.LoggingConfiguration
import org.scalatest.{Matchers, FunSuite}

class TestRedBlackTree extends FunSuite with TreeTestTrait with Matchers with LoggingConfiguration {

  test("Red black tree") {
    val empty = new RedBlackTree[String, String, Unit](None, new SetItemKind[String] {})
    testTree(empty, Red, Black)
  }
}
