package org.leialearns.crystallize.immutabletree

trait TreeTestTrait {

  def testCreateNode[A,T,V](tree: Tree[A,A,Unit,T,V], value: V, leftNodeOption: Option[TreeNodeTrait[A,T] with T], item: A, rightNodeOption: Option[TreeNodeTrait[A,T] with T]): TreeNodeTrait[A,T] with T = {
    val result = testCreateNode(tree, value, leftNodeOption, Left(item), rightNodeOption)
    assert(result.getItem == item)
    result
  }
  def testCreateNode[A,T,V](tree: Tree[A,A,Unit,T,V], value: V, leftNodeOption: Option[TreeNodeTrait[A,T] with T], bucket: TreeNodeTrait[A,T] with T, rightNodeOption: Option[TreeNodeTrait[A,T] with T]): TreeNodeTrait[A,T] with T = {
    val result = testCreateNode(tree, value, leftNodeOption, Right(bucket), rightNodeOption)
    result
  }
  def testCreateNode[A,T,V](tree: Tree[A,A,Unit,T,V], value: V, leftNodeOption: Option[TreeNodeTrait[A,T] with T], middle: Either[A,TreeNodeTrait[A,T] with T], rightNodeOption: Option[TreeNodeTrait[A,T] with T]): TreeNodeTrait[A,T] with T = {
    val result = tree.createNode(leftNodeOption, middle, rightNodeOption, value)
    if (leftNodeOption.isEmpty && middle.isRight && rightNodeOption.isEmpty) {
      assert(result == middle.right.get)
    } else {
      val givenLeftEither = leftNodeOption map (asEither[A,T](_))
      val givenRightEither = rightNodeOption map (asEither[A,T](_))
      assert(result.getLeftNode == givenLeftEither)
      assert(result.getRightNode == givenRightEither)
      assert(result.getMiddle == middle)
      middle match {
        case Left(middleItem) =>
          assert(result.getItem == middleItem)
          assert(result.getBucket.isEmpty)
        case Right(middleBucket) =>
          assert(result.getItem == middleBucket.getItem)
          assert(result.getBucket == Some(middleBucket))
      }
    }
    result
  }
  def asTree[A,T,V](tree: Tree[A,A,Unit,T,V], value: V, either: Either[A,TreeNodeTrait[A,T] with T]): TreeNodeTrait[A,T] with T = {
    either match {
      case Left(item) => tree.createNode(None, item, None, value)
      case Right(node) => node
    }
  }
  def asEither[A,T](node: TreeNodeTrait[A,T]): Either[A,TreeNodeTrait[A,T]] = {
    (node.getLeftNode, node.getMiddle, node.getRightNode) match {
      case (None, Left(item), None) => Left(item)
      // case (None, Right(bucket), None) => Right(bucket)
      case _ => Right(node)
    }
  }

  def testTree[T,V](empty: Tree[String,String,Unit,T,V], value: V, value2: V) = {

    // Item nodes
    val n1 = testCreateNode(empty, value, None, "one", None)
    val n2 = testCreateNode(empty, value, None, "two", None)
    val n3 = testCreateNode(empty, value, Some(n1), "three", None)
    val n4 = testCreateNode(empty, value, None, "four", Some(n2))
    val n5 = testCreateNode(empty, value, Some(n1), "five", Some(n2))
    val n6 = testCreateNode(empty, value, Some(n3), "five", None)
    val n7 = testCreateNode(empty, value, None, "five", Some(n4))
    val n8 = testCreateNode(empty, value, Some(n3), "five", Some(n4))

    // Bucket nodes
    val n11 = testCreateNode(empty, value2, None, n8, None)
    val n12 = testCreateNode(empty, value2, None, n8, None)
    val n13 = testCreateNode(empty, value2, Some(n1), n8, None)
    val n14 = testCreateNode(empty, value2, None, n8, Some(n2))
    val n15 = testCreateNode(empty, value2, Some(n1), n8, Some(n2))
    val n16 = testCreateNode(empty, value2, Some(n3), n8, None)
    val n17 = testCreateNode(empty, value2, None, n8, Some(n4))
    val n18 = testCreateNode(empty, value2, Some(n3), n8, Some(n4))

    val n23 = testCreateNode(empty, value, Some(n11), n8, None)
    val n24 = testCreateNode(empty, value, None, n8, Some(n12))
    val n25 = testCreateNode(empty, value, Some(n11), n8, Some(n2))
    val n35 = testCreateNode(empty, value, Some(n1), n8, Some(n12))

    val one = empty.insert("one")
    assert(Some("one") == one.find("one"))
    assert(None == one.find("two"))

    val two = one.insert("two")
    assert(Some("one") == two.find("one"))
    assert(Some("two") == two.find("two"))
    assert(None == two.find("three"))
  }
}
