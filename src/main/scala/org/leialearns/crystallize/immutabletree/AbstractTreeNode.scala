package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

trait NodeFactory[A,T <: TreeNodeTrait[A,T],V] {
  def createNode(leftNodeOption: Option[T], item: A, rightNodeOption: Option[T], variant: V): T
  def createNode(leftNodeOption: Option[T], bucket: T, rightNodeOption: Option[T], variant: V): T
  def createNode(leftNodeOption: Option[T], middle: Either[A,T], rightNodeOption: Option[T], variant: V): T = {
    middle match {
      case Left(item) => createNode(leftNodeOption, item, rightNodeOption, variant)
      case Right(tree) => createNode(leftNodeOption, tree, rightNodeOption, variant)
    }
  }
  def asTree(either: Either[A,T], variant: V): T = {
    either match {
      case Left(item) => createNode(None, item, None, variant)
      case Right(tree) => tree
    }
  }

}

abstract class Tree[A, T <: TreeNodeTrait[A,T], V](_rootOption: Option[T], _nodeFactory: NodeFactory[A,T,V]) extends Logging {
  def getNodeFactory: NodeFactory[A,T,V] = _nodeFactory
  def getRoot: Option[T] = _rootOption
  def createNode(leftNodeOption: Option[T], middle: Either[A,T], rightNodeOption: Option[T], variant: V): T = {
    val result = getNodeFactory.createNode(leftNodeOption, middle, rightNodeOption, variant)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[T], bucket: T, rightNodeOption: Option[T], variant: V): T = {
    val result = getNodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, variant)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[T], item: A, rightNodeOption: Option[T], variant: V): T = {
    val result: T = getNodeFactory.createNode(leftNodeOption, item, rightNodeOption, variant)
    trace(s"Created item node: ${dump(result, Nil)}")
    result
  }
  def createItemNode(variant: V, item: A): T = {
    createNode(None, item, None, variant)
  }
  def insert(item: A): Tree[A,T,V]
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

trait TreeNodeTrait[+A, +T <: TreeNodeTrait[A,T]] {
  def getLeftNode: Option[Either[A,T]]
  def getMiddle: Either[A,T]
  def getBucket: Option[T]
  def getItem: A
  def getItems = getBucket map (_.getAllItems) getOrElse (getItem :: Nil)
  def getAllItems: Iterable[A] = { getItems ++ ((getLeftNode :: getRightNode :: Nil).flatten flatMap (getAllItems(_))) }
  def getAllItems[B >: A, T2 <: TreeNodeTrait[B,T2]](branch: Either[B,T2]): Iterable[B] = {
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
  def eitherToTree[T2 <: TreeNodeTrait[_,T2]](either: Either[_,T2]): Option[T2] = {
    either match {
      case Right(tree) => Some(tree)
      case _ => None
    }
  }
}

abstract sealed class AbstractTreeNode[+A,+T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] {
}

// No branches
case class ItemNode[A,T <: TreeNodeTrait[A,T]](item: A) extends TreeNodeTrait[A,T] {
  override def getLeftNode = None
  override def getMiddle = Right(this.asInstanceOf[T])
  override def getBucket = Some(this.asInstanceOf[T])
  override def getItem = item
  override def getItems = item :: Nil
  override def getRightNode = None
}

class ShiftedNode[A,T <: TreeNodeTrait[A,T]](node: TreeNodeTrait[A,T]) extends TreeNodeTrait[A,T] {
  override def getOffset = ZERO
  override def getLeftNode = Some(node.getMiddle)
  override def getMiddle = node.getRightNode.get
  override def getBucket = eitherToTree(node.getRightNode.get)
  override def getItem = node.getRightNode.get match {
    case Left(item) => item
    case Right(tree) => tree.getItem
  }
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

trait SingleNodeTrait[+M] {
  def getMiddleRaw: M
}

abstract class SingleNode[M,+A,+T <: TreeNodeTrait[A,T]](content: M) extends AbstractTreeNode[A,T] with SingleNodeTrait[M] {
  override def getLeftNode = None
  override def getRightNode = None
  def getMiddleRaw = content
}
trait Pair[L,R,A,T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] {
  def left: L
  override def getMiddle: Either[A,T] = {
    getBucket match {
      case Some(bucket) => Right(bucket)
      case _ => Left(getItem)
    }
  }
  def right: R
}
abstract class PairNode[L,R,A,T <: TreeNodeTrait[A,T]](_left: L, _right: R) extends Pair[L,R,A,T] {
  def left = _left
  def right = _right
}

trait LeftItem[+A,+T <: TreeNodeTrait[A,T]] extends Pair[A,_,A,T] {
  override def getLeftNode = Some(Left(left))
  override def getRightNode = None
}
trait LeftTree[+A,+T <: TreeNodeTrait[A,T]] extends Pair[T,_,A,T] {
  override def getLeftNode = Some(Right(left))
  override def getRightNode = None
}
trait LeftNodeItem[+A,+T <: TreeNodeTrait[A,T]] extends Pair[_,A,A,T] {
  override def getBucket = None
  override def getItem = right
}
trait LeftNodeBucket[+A,+T <: TreeNodeTrait[A,T]] extends Pair[_,T,A,T] {
  override def getBucket = Some(right)
  override def getItem = right.getItem
}

trait RightItem[+A,+T <: TreeNodeTrait[A,T]] extends Pair[_,A,A,T] {
  override def getLeftNode = None
  override def getRightNode = Some(Left(right))
}
trait RightTree[+A,+T <: TreeNodeTrait[A,T]] extends Pair[_,T,A,T] {
  override def getLeftNode = None
  override def getRightNode = Some(Right(right))
}
trait RightNodeItem[+A,+T <: TreeNodeTrait[A,T]] extends Pair[A,_,A,T] {
  override def getBucket = None
  override def getItem = left
}
trait RightNodeBucket[+A,+T <: TreeNodeTrait[A,T]] extends Pair[T,_,A,T] {
  override def getBucket = Some(left)
  override def getItem = left.getItem
}

abstract class BothNodes[L,M,R,A,T <: TreeNodeTrait[A,T]](_left: L, _content: M, _right: R) extends PairNode[L,R,A,T](_left,_right) with SingleNodeTrait[M] {
  override def left = _left
  override def right = _right

  override def getMiddle = {
    getBucket match {
      case Some(bucket) => Right(bucket)
      case _ => Left(getItem)
    }
  }
  override def getMiddleRaw = _content
}
trait Bucket[+A,+T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] with SingleNodeTrait[T] {
  override def getMiddle = Right(getMiddleRaw)
  override def getBucket = Some(getMiddleRaw)
  override def getItem = getMiddleRaw.getItem
}
trait Item[+A,+T <: TreeNodeTrait[A,T]] extends TreeNodeTrait[A,T] with SingleNodeTrait[A] {
  override def getMiddle = Left(getMiddleRaw)
  override def getBucket = None
  override def getItem = getMiddleRaw
}
