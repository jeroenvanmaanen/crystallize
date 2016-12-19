package org.leialearns.crystalize.immutabletree

import grizzled.slf4j.Logging

import collection.immutable.Seq

abstract class Tree[A, C](_rootOption: Option[AbstractTreeNode[A]]) extends Logging {
  def getRoot: Option[AbstractTreeNode[A]] = _rootOption
  def createNode(leftNodeOption: Option[AbstractTreeNode[A]], bucket: AbstractTreeNode[A], rightNodeOption: Option[AbstractTreeNode[A]], context: C): AbstractTreeNode[A] = {
    val result = (leftNodeOption, bucket, rightNodeOption) match {
      case (None, ItemNode(item), None) => bucket
      case (_, ItemNode(item), _) => createNode(leftNodeOption, item, rightNodeOption, context)
      case _ =>
        (leftNodeOption, rightNodeOption) match {
          case (Some(ItemNode(leftItem)), Some(ItemNode(rightItem))) => createItemBucketItemNode(context, RIGHT, ZERO, leftItem, bucket, rightItem)
          case (None, Some(ItemNode(rightItem))) => createBucketItemNode(context, RIGHT, ZERO, bucket, rightItem)
          case (Some(ItemNode(leftItem)), None) => createBucketItemNode(context, LEFT, ZERO, bucket, leftItem)
          case (Some(leftNode), Some(ItemNode(rightItem))) => createSubtreeBucketItemNode(context, RIGHT, ZERO, leftNode, bucket, rightItem)
          case (Some(ItemNode(leftItem)), Some(rightNode)) => createSubtreeBucketItemNode(context, LEFT, ZERO, rightNode, bucket, leftItem)
          case (Some(leftNode), Some(rightNode)) => createSubtreeBucketSubtreeNode(context, leftNode, bucket, rightNode)
          case (Some(leftNode), None) => createBucketSubtreeNode(context, ONE, leftNode, bucket)
          case (None, Some(rightNode)) => createBucketSubtreeNode(context, ZERO, bucket, rightNode)
          case _ => bucket
        }
    }
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[AbstractTreeNode[A]], item: A, rightNodeOption: Option[AbstractTreeNode[A]], context: C): AbstractTreeNode[A] = {
    val result = (leftNodeOption, rightNodeOption) match {
      case (Some(ItemNode(leftItem)), Some(ItemNode(rightItem))) => createItemItemItemNode(context, leftItem, item, rightItem)
      case (None, Some(ItemNode(rightItem))) => createItemItemNode(context, ZERO, item, rightItem)
      case (Some(ItemNode(leftItem)), None) => createItemItemNode(context, ONE, leftItem, item)
      case (Some(leftNode), Some(ItemNode(rightItem))) => createItemBucketItemNode(context, RIGHT, ONE, rightItem, leftNode, item)
      case (Some(ItemNode(leftItem)), Some(rightNode)) => createItemBucketItemNode(context, LEFT, ONE, leftItem, rightNode, item)
      case (Some(leftNode), Some(rightNode)) => createSubtreeBucketItemNode(context, LEFT, ONE, leftNode, rightNode, item)
      case (Some(leftNode), None) => createBucketItemNode(context, RIGHT, ONE, leftNode, item)
      case (None, Some(rightNode)) => createBucketItemNode(context, LEFT, ONE, rightNode, item)
      case _ => createItemNode(context, item)
    }
    trace(s"Created item node: ${dump(result, Nil)}")
    result
  }
  def createItemNode(context: C, item: A): ItemNode[A] = new ItemNode[A](item)
  def createItemItemNode(context: C, offset: Offset, item: A, right: A) = new ItemItemNode[A](offset, item, right)
  def createBucketItemNode(context: C, orientation: Orientation, offset: Offset, bucket: AbstractTreeNode[A], right: A) = new BucketItemNode[A](orientation, offset, bucket, right)
  def createBucketSubtreeNode(context: C, offset: Offset, bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) = new BucketSubtreeNode[A](offset, bucket, right)
  def createItemItemItemNode(context: C, left: A, item: A, right: A) = new ItemItemItemNode[A](left, item, right)
  def createItemBucketItemNode(context: C, orientation: Orientation, offset: Offset, left: A, bucket: AbstractTreeNode[A], right: A) = new ItemBucketItemNode[A](orientation, offset, left, bucket, right)
  def createSubtreeBucketItemNode(context: C, orientation: Orientation, offset: Offset, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: A) = new SubtreeBucketItemNode[A](orientation, offset, left, bucket, right)
  def createSubtreeBucketSubtreeNode(context: C, left: AbstractTreeNode[A], bucket: AbstractTreeNode[A], right: AbstractTreeNode[A]) = new SubtreeBucketSubtreeNode[A](left, bucket, right)
  def insert(item: A): Tree[A, C]
  def dump: String = {
    s"<t>${dump(getRoot, Nil)}</t>"
  }
  def dump(nodeOption: Option[AbstractTreeNode[A]], rootPath: List[AnyRef]): String = {
    nodeOption match {
      case Some(node) => dump(node, rootPath)
      case _ => "<n/>"
    }
  }
  def dump(node: AbstractTreeNode[A], rootPath: List[AnyRef]): String = {
    if (rootPath contains node) {
      s"<!-- ${node.toString} -->"
    } else {
      val subpath = node :: rootPath
      val untwisted = node.untwist
      node match {
        case ItemNode(item) => item.toString
        case _ => s"<n>${dump(untwisted.getLeftNode, subpath)}<i>${dump(untwisted.getBucket, subpath)}</i>${dump(untwisted.getRightNode, subpath)}</n>"
      }
    }
  }
}

abstract sealed class Offset {}
case object ZERO extends Offset
case object ONE extends Offset

abstract sealed class Orientation {}
case object RIGHT extends Orientation {}
case object LEFT extends Orientation {}

abstract sealed class AbstractTreeNode[A] {
  def getLeftNode: Option[AbstractTreeNode[A]]
  def getBucket: AbstractTreeNode[A]
  def getItems: Seq[A]
  def getItem: A
  def getAllItems: Seq[A] = getItems ++ ((getLeftNode :: getRightNode :: Nil) flatMap { case n => getAllItems })
  def getRightNode: Option[AbstractTreeNode[A]]
  def getOrientation: Orientation = RIGHT
  def getOffset: Offset = ZERO
  def untwist: AbstractTreeNode[A] = {
    val shifted = if (getOffset == ONE) new ShiftedNode[A](this) else this
    if (getOrientation == LEFT) new MirroredNode[A](shifted) else shifted
  }
}

// No branches
case class ItemNode[A](item: A) extends AbstractTreeNode[A] {
  override def getLeftNode = None
  override def getBucket = this
  override def getItem = item
  override def getItems = item :: Nil
  override def getRightNode = None
}

// Right branch
case class ItemItemNode[A](offset: Offset, item: A, right: A) extends AbstractTreeNode[A] {
  override def getLeftNode = None
  override def getBucket = new ItemNode[A](item)
  override def getItem = item
  override def getItems = item :: Nil
  override def getRightNode = Some(new ItemNode[A](right))
  override def getOffset = offset
}
case class BucketItemNode[A](orientation: Orientation, offset: Offset, item: AbstractTreeNode[A], right: A) extends AbstractTreeNode[A] {
  override def getLeftNode = None
  override def getBucket = item
  override def getItem = item.getItem
  override def getItems = item.getAllItems
  override def getRightNode = Some(new ItemNode[A](right))
  override def getOrientation = orientation
  override def getOffset = offset
}
case class BucketSubtreeNode[A](offset: Offset, item: AbstractTreeNode[A], right: AbstractTreeNode[A]) extends AbstractTreeNode[A] {
  override def getLeftNode = None
  override def getBucket = item
  override def getItem = item.getItem
  override def getItems = item.getAllItems
  override def getRightNode = Some(right)
  override def getOffset = offset
}

// Both branches
case class ItemItemItemNode[A](left: A, item: A, right: A) extends AbstractTreeNode[A] {
  override def getLeftNode = Some(new ItemNode[A](left))
  override def getBucket = new ItemNode[A](item)
  override def getItem = item
  override def getItems = item :: Nil
  override def getRightNode = Some(new ItemNode[A](right))
}
case class ItemBucketItemNode[A](orientation: Orientation, offset: Offset, left: A, item: AbstractTreeNode[A], right: A) extends AbstractTreeNode[A] {
  override def getLeftNode = Some(new ItemNode[A](left))
  override def getBucket = item
  override def getItem = item.getItem
  override def getItems = item.getAllItems
  override def getRightNode = Some(new ItemNode[A](right))
  override def getOrientation = orientation
  override def getOffset = offset
}
case class SubtreeBucketItemNode[A](orientation: Orientation, offset: Offset, left: AbstractTreeNode[A], item: AbstractTreeNode[A], right: A) extends AbstractTreeNode[A] {
  override def getLeftNode = Some(left)
  override def getBucket = item
  override def getItem = item.getItem
  override def getItems = item.getAllItems
  override def getRightNode = Some(new ItemNode[A](right))
  override def getOrientation = orientation
  override def getOffset = offset
}
case class SubtreeBucketSubtreeNode[A](left: AbstractTreeNode[A], item: AbstractTreeNode[A], right: AbstractTreeNode[A]) extends AbstractTreeNode[A] {
  override def getLeftNode = Some(left)
  override def getBucket = item
  override def getItem = item.getItem
  override def getItems = item.getAllItems
  override def getRightNode = Some(right)
}

class ShiftedNode[A](node: AbstractTreeNode[A]) extends AbstractTreeNode[A] {
  override def getOffset = ZERO
  override def getLeftNode = Some(node.getBucket)
  override def getBucket = node.getRightNode.get
  override def getItem = getBucket.getItem
  override def getItems = getBucket.getAllItems
  override def getRightNode = node.getLeftNode
  override def toString = s"ShiftedNode($node})"
}
class MirroredNode[A](node: AbstractTreeNode[A]) extends AbstractTreeNode[A] {
  override def getOrientation = RIGHT
  override def getLeftNode = node.getRightNode
  override def getBucket = node.getBucket
  override def getItem = node.getItem
  override def getItems = node.getAllItems
  override def getRightNode = node.getLeftNode
  override def toString = s"MirroredNode($node})"
}
