package org.leialearns.crystallize.immutabletree

class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[AbstractTreeNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, NodeColor](rootOption) {
  override def createItemNode(color: NodeColor, item: A): ItemNode[A] = new RedBlackItemNode[A](color, item)
  override def createItemItemNode(color: NodeColor, offset: Offset, item: A, right: A) = new RedBlackItemItemNode[A](color, offset, item, right)
  override def createBucketItemNode(color: NodeColor, orientation: Orientation, offset: Offset, bucket: AbstractTreeNode[A], right: A) = new RedBlackBucketItemNode[A](color, orientation, offset, bucket, right)
  override def createBucketSubtreeNode(color: NodeColor, offset: Offset, bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) = new RedBlackBucketSubtreeNode[A](color, offset, bucket, right)
  override def createItemItemItemNode(color: NodeColor, left: A, item: A, right: A) = new RedBlackItemItemItemNode[A](color, left, item, right)
  override def createItemBucketItemNode(color: NodeColor, orientation: Orientation, offset: Offset, left: A, bucket: AbstractTreeNode[A], right: A) = new RedBlackItemBucketItemNode[A](color, orientation, offset, left, bucket, right)
  override def createSubtreeBucketItemNode(color: NodeColor, orientation: Orientation, offset: Offset, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: A) = new RedBlackSubtreeBucketItemNode[A](color, orientation, offset, left, bucket, right)
  override def createSubtreeBucketSubtreeNode(color: NodeColor, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) = new RedBlackSubtreeBucketSubtreeNode[A](color, left, bucket, right)
  override def insert(item: A): Tree[A, NodeColor] = ???
}
abstract sealed class NodeColor
case class Red() extends NodeColor
case class Black() extends NodeColor
trait RedBlackNode {
  def getColor: NodeColor
}

class RedBlackItemNode[A](color: NodeColor, item: A) extends ItemNode[A](item) with RedBlackNode {
  override def getColor = color
}
class RedBlackItemItemNode[A](color: NodeColor, offset: Offset, item: A, right: A) extends ItemItemNode[A](offset, item, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackBucketItemNode[A](color: NodeColor, orientation: Orientation, offset: Offset, bucket: AbstractTreeNode[A], right: A) extends BucketItemNode[A](orientation, offset, bucket, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackBucketSubtreeNode[A](color: NodeColor, offset: Offset, bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) extends BucketSubtreeNode[A](offset, bucket, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackItemItemItemNode[A](color: NodeColor, left: A, item: A, right: A) extends ItemItemItemNode[A](left, item, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackItemBucketItemNode[A](color: NodeColor, orientation: Orientation, offset: Offset, left: A, bucket: AbstractTreeNode[A], right: A) extends ItemBucketItemNode[A](orientation, offset, left, bucket, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackSubtreeBucketItemNode[A](color: NodeColor, orientation: Orientation, offset: Offset, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: A) extends SubtreeBucketItemNode[A](orientation, offset, left, bucket, right) with RedBlackNode {
  override def getColor = color
}
class RedBlackSubtreeBucketSubtreeNode[A](color: NodeColor, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) extends SubtreeBucketSubtreeNode[A](left, bucket, right) with RedBlackNode {
  override def getColor = color
}
