package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

trait NodeFactory[A,T <: TreeNodeTrait[A,T]] {
  def createNode(leftNodeOption: Option[T], middle: Either[A,T], rightNodeOption: Option[T]): T
  def createNode(leftNodeOption: Option[T], bucket: T, rightNodeOption: Option[T]): T
  def createNode(leftNodeOption: Option[T], item: A, rightNodeOption: Option[T]): T
  def asTree(either: Either[A,T]): T = {
    either match {
      case Left(item) => createNode(None, item, None)
      case Right(tree) => tree
    }
  }

}

abstract class Tree[A, C, T <: TreeNodeTrait[A,T]](_rootOption: Option[T], _nodeFactory: NodeFactory[A, T]) extends Logging {
  def getNodeFactory: NodeFactory[A,T] = _nodeFactory
  def getRoot: Option[T] = _rootOption
  def createNode(leftNodeOption: Option[T], middle: Either[A,T], rightNodeOption: Option[T], context: C): T = {
    val result = getNodeFactory.createNode(leftNodeOption, middle, rightNodeOption)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[T], bucket: T, rightNodeOption: Option[T], context: C): T = {
    val result = getNodeFactory.createNode(leftNodeOption, bucket, rightNodeOption)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[T], item: A, rightNodeOption: Option[T], context: C): T = {
    val result: T = getNodeFactory.createNode(leftNodeOption, item, rightNodeOption)
    trace(s"Created item node: ${dump(result, Nil)}")
    result
  }
  def createItemNode(context: C, item: A): T = {
    createNode(None, item, None, context)
  }
  def insert(item: A): Tree[A, C, T]
  def dump: String = {
    s"<t>${dump(getRoot map (Right(_)), Nil)}</t>"
  }
  def dump(nodeOption: Option[Either[A,T]], rootPath: List[AnyRef]): String = {
    nodeOption match {
      case Some(Left(item)) => s"<n>$item</n>"
      case Some(Right(node)) => dump(node, rootPath)
      case _ => "<n/>"
    }
  }
  def dump(node: T, rootPath: List[AnyRef]): String = {
    if (rootPath contains node) {
      s"<!-- ${node.toString} -->"
    } else {
      val subpath = node :: rootPath
      val untwisted = node.untwist
      (untwisted.getLeftNode, untwisted.getMiddle, untwisted.getRightNode) match {
        case (None, Left(item), None) => item.toString
        case (left, Left(item), right) => s"<n>${dump(left, subpath)}<i>$item</i>${dump(right, subpath)}</n>"
        case (left, Right(bucket), right) => s"<n>${dump(left, subpath)}<i>${dump(bucket, subpath)}</i>${dump(right, subpath)}</n>"
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

trait TreeNodeTrait[A, T <: TreeNodeTrait[A,T]] {
  def getLeftNode: Option[Either[A,T]]
  def getMiddle: Either[A,T]
  def getBucket: Option[T]
  def getItem: Option[A]
  def getItems: Iterable[A]
  def getAllItems: Iterable[A] = { getItems ++ ((getLeftNode :: getRightNode :: Nil).flatten flatMap (getAllItems(_))) }
  def getAllItems(branch: Either[A,T]): Iterable[A] = {
    branch match {
      case Left(item) => item :: Nil
      case Right(tree) => tree.getAllItems
    }
  }
  def getRightNode: Option[Either[A,T]]
  def getOrientation: Orientation = RIGHT
  def getOffset: Offset = ZERO
  def untwist: T = {
    this.asInstanceOf[T]
  }
  def eitherToTree(either: Option[Either[_,T]]): Option[T] = {
    either match {
      case Some(Right(tree)) => Some(tree)
      case _ => None
    }
  }
}

abstract sealed class AbstractTreeNode[A,T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] {
}

// No branches
case class ItemNode[A,T <: TreeNodeTrait[A,T]](item: A) extends TreeNodeTrait[A,T] {
  override def getLeftNode = None
  override def getMiddle = Right(this.asInstanceOf[T])
  override def getBucket = Some(this.asInstanceOf[T])
  override def getItem = Some(item)
  override def getItems = item :: Nil
  override def getRightNode = None
}

class ShiftedNode[A,T <: TreeNodeTrait[A,T]](node: TreeNodeTrait[A,T]) extends TreeNodeTrait[A,T] {
  override def getOffset = ZERO
  override def getLeftNode = Some(node.getMiddle)
  override def getMiddle = node.getRightNode.get
  override def getBucket = eitherToTree(node.getRightNode)
  override def getItem = getBucket flatMap (_.getItem)
  override def getItems = getBucket map (_.getAllItems) getOrElse Nil
  override def getRightNode = node.getLeftNode
  override def toString = s"ShiftedNode($node})"
}
class MirroredNode[A,T <: TreeNodeTrait[A,T]](node: TreeNodeTrait[A,T]) extends TreeNodeTrait[A,T] {
  override def getOrientation = RIGHT
  override def getLeftNode = node.getRightNode
  override def getMiddle = node.getMiddle
  override def getBucket = node.getBucket
  override def getItem = node.getItem
  override def getItems = node.getAllItems
  override def getRightNode = node.getLeftNode
  override def toString = s"MirroredNode($node})"
}

trait SingleNodeTrait[M,A,T] {
  def asTreeNode: M => Option[T]
  def asItem: M => Option[A]
}

abstract class SingleNode[M,A,T <: TreeNodeTrait[A,T]](content: M) extends AbstractTreeNode[A,T] with SingleNodeTrait[M, A, T] {
  override def getLeftNode = None
  override def getMiddle = {
    (getItem, getBucket) match {
      case (Some(item), _) => Left(item)
      case (_, Some(bucket)) => Right(bucket)
      case _ => throw  new IllegalStateException(s"Node has neither Item nor Bucket: $this")
    }
  }
  override def getBucket = asTreeNode(content)
  override def getItem = asItem(content)
  override def getItems = getBucket map (_.getAllItems) getOrElse Nil
  override def getRightNode = None
}
trait Pair[L,R,A,T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] {
  def leftAsItem: L => Option[A]
  def rightAsItem: R => Option[A]
  def leftAsTreeNode: L => Option[Either[A,T]]
  def rightAsTreeNode: R => Option[Either[A,T]]
  def left: L
  def getMiddle: Either[A,T] = {
    (getItem, getBucket) match {
      case (Some(item), _) => Left(item)
      case (_, Some(bucket)) => Right(bucket)
      case _ => throw new IllegalStateException(s"Node has neither Item nor Bucket: $this")
    }
  }
  def right: R
}
abstract class PairNode[L,R,A,T <: TreeNodeTrait[A,T]](_left: L, _right: R) extends Pair[L,R,A,T] {
  def left = _left
  def right = _right
}
trait LeftNode[L,M,A,T <: TreeNodeTrait[A,T]] extends Pair[L,M,A,T] {
  override def getLeftNode = leftAsTreeNode(left)
  override def getBucket = eitherToTree(rightAsTreeNode(right))
  override def getItem = rightAsItem(right)
  override def getItems = getBucket map (_.getAllItems) getOrElse Nil
  override def getRightNode = None
}
trait RightNode[M,R,A,T <: TreeNodeTrait[A,T]] extends Pair[M,R,A,T] {
  override def getLeftNode = None
  override def getBucket = eitherToTree(leftAsTreeNode(left))
  override def getItem = leftAsItem(left)
  override def getItems = getBucket map (_.getAllItems) getOrElse Nil
  override def getRightNode = rightAsTreeNode(right)
}

trait BothNodesTrait[L,M,R,A,T <: TreeNodeTrait[A,T]] {
  def asTreeNode: M => Option[T]
  def asItem: M => Option[A]
}
abstract class BothNodes[L,M,R,A,T <: TreeNodeTrait[A,T]](_left: L, content: M, _right: R) extends PairNode[L,R,A,T](_left,_right) with BothNodesTrait[L,M,R,A,T] {
  override def getLeftNode = leftAsTreeNode(left)
  override def getMiddle = {
    (getItem, getBucket) match {
      case (Some(item), _) => Left(item)
      case (_, Some(bucket)) => Right(bucket)
      case _ => throw  new IllegalStateException(s"Node has neither Item nor Bucket: $this")
    }
  }
  override def getBucket = asTreeNode(content)
  override def getItem = asItem(content)
  override def getItems = getBucket map (_.getAllItems) getOrElse Nil
  override def getRightNode = rightAsTreeNode(right)
}
trait Bucket[A,T <: TreeNodeTrait[A,T]] extends SingleNodeTrait[T,A,T] {
  override def asTreeNode = Some(_)
  override def asItem = Function.const(None)
}
trait Item[A,T <: TreeNodeTrait[A,T]] extends SingleNodeTrait[A,A,T] {
  override def asTreeNode = Function.const(None)
  override def asItem = Some(_)
}
trait LeftTree[L,A,T <: TreeNodeTrait[A,T]] extends Pair[L,T,A,T] {
  override def getLeftNode = leftAsTreeNode(left)
  override def getBucket = eitherToTree(rightAsTreeNode(right))
  override def getItem = right.getItem
  override def getItems = right.getAllItems
  override def getRightNode = None
}
trait LeftItem[L,A,T <: TreeNodeTrait[A,T]] extends Pair[L,A,A,T] {
  override def getLeftNode = leftAsTreeNode(left)
  override def getBucket = eitherToTree(rightAsTreeNode(right))
  override def getItem = Some(right)
  override def getItems = right :: Nil
  override def getRightNode = None
}
trait RightTree[A,R,T <: TreeNodeTrait[A,T]] extends Pair[T,R,A,T] {
  override def getLeftNode = None
  override def getBucket = eitherToTree(leftAsTreeNode(left))
  override def getItem = left.getItem
  override def getItems = left.getAllItems
  override def getRightNode = rightAsTreeNode(right)
}
trait RightItem[A,R,T <: TreeNodeTrait[A,T]] extends Pair[A,R,A,T] {
  override def getLeftNode = None
  override def getBucket = eitherToTree(leftAsTreeNode(left))
  override def getItem = Some(left)
  override def getItems = left :: Nil
  override def getRightNode = rightAsTreeNode(right)
}
trait CoerceLeftTree[A,R,T <: TreeNodeTrait[A,T]] extends Pair[T,R,A,T] {
  override def leftAsTreeNode = (tree) => Some(Right(tree))
  override def leftAsItem = Function.const(None)
}
trait CoerceLeftItem[A,R,T <: TreeNodeTrait[A,T]] extends Pair[A,R,A,T] {
  override def leftAsTreeNode = (item) => Some(Left(item))
  override def leftAsItem = Some(_)
}
trait CoerceMiddleBucket[L,A,R,T <: TreeNodeTrait[A,T]] extends BothNodesTrait[L,T,R,A,T] {
  override def asTreeNode = Some(_)
  override def asItem = Function.const(None)
}
trait CoerceMiddleItem[L,A,R,T <: TreeNodeTrait[A,T]] extends BothNodesTrait[L,A,R,A,T] {
  override def asTreeNode = Function.const(None)
  override def asItem = Some(_)
}
trait CoerceRightTree[L,A,T <: TreeNodeTrait[A,T]] extends Pair[L,T,A,T] {
  override def rightAsTreeNode = (tree) => Some(Right(tree))
  override def rightAsItem = Function.const(None)
}
trait CoerceRightItem[L,A,T <: TreeNodeTrait[A,T]] extends Pair[L,A,A,T] {
  override def rightAsTreeNode = (item) => Some(Left(item))
  override def rightAsItem = Some(_)
}
