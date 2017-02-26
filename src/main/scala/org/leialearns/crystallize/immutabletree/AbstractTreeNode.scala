package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

trait NodeFactory[A,T,V] {
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], item: A, rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: V): TreeNodeTrait[A,T] with T
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], bucket: TreeNodeTrait[A,T] with T, rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: V): TreeNodeTrait[A,T] with T
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], middle: Either[A,TreeNodeTrait[A,T] with T], rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: V): TreeNodeTrait[A,T] with T = {
    middle match {
      case Left(item) => createNode(leftNodeOption, item, rightNodeOption, variant)
      case Right(tree) => createNode(leftNodeOption, tree, rightNodeOption, variant)
    }
  }
  def asTree[A2 <: A,T2 <: T](either: Either[A2,T2], variant: V): T = {
    either match {
      case Left(item) => createNode(None, item, None, variant)
      case Right(tree) => tree
    }
  }

}

abstract class Tree[A, K, V, T, C](_rootOption: Option[TreeNodeTrait[A,T] with T], _nodeFactory: NodeFactory[A,T,C], _itemKind: ItemKind[A,K,V]) extends Logging {
  def getItemKind = _itemKind
  def getNodeFactory: NodeFactory[A,T,C] = _nodeFactory
  def getRoot: Option[TreeNodeTrait[A,T] with T] = _rootOption
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], middle: Either[A,TreeNodeTrait[A,T] with T], rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: C): TreeNodeTrait[A,T] with T = {
    val result = getNodeFactory.createNode(leftNodeOption, middle, rightNodeOption, variant)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], bucket: TreeNodeTrait[A,T] with T, rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: C): TreeNodeTrait[A,T] with T = {
    val result = getNodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, variant)
    trace(s"Created node: ${dump(result, Nil)}")
    result
  }
  def createNode(leftNodeOption: Option[TreeNodeTrait[A,T] with T], item: A, rightNodeOption: Option[TreeNodeTrait[A,T] with T], variant: C): TreeNodeTrait[A,T] with T = {
    val result: TreeNodeTrait[A,T] with T = getNodeFactory.createNode(leftNodeOption, item, rightNodeOption, variant)
    trace(s"Created item node: ${dump(result, Nil)}")
    result
  }
  def createItemNode(variant: C, item: A): T = {
    createNode(None, item, None, variant)
  }
  def insert(item: A): Tree[A,K,V,T,C]
  def find(key: K): Option[A] = {
    find(_rootOption map (Right(_)), key)
  }
  def find(nodeOption: Option[Either[A,TreeNodeTrait[A,T]]], key: K): Option[A] = {
    nodeOption flatMap (find(_, key))
  }
  def extractKey(bucket: TreeNodeTrait[A,T]): K = {
    bucket.getMiddle match {
      case Left(item) => getItemKind.getKey(item)
      case Right(child) =>
        if (child eq bucket) throw new IllegalStateException("Non-item bucket is its own bucket") else extractKey(child)
    }
  }
  def find(either: Either[A,TreeNodeTrait[A,T]], key: K): Option[A] = {
    either match {
      case Left(item) => lookup(item, key)
      case Right(tree) => find(tree, key)
    }
  }
  def find(node: TreeNodeTrait[A,T], key: K): Option[A] = {
    val order = getItemKind.compare(key, extractKey(node))
    if (order < 0) {
      find(node.getLeftNode, key)
    } else if (order > 0) {
      find(node.getRightNode, key)
    } else {
      lookup(Right(node), key)
    }
  }
  def lookup(nodeOption: Option[Either[A,TreeNodeTrait[A,T]]], key: K): Option[A] = {
    nodeOption flatMap (lookup(_, key))
  }
  def lookup(item: A, key: K): Option[A] = if (getItemKind.equals(key, getItemKind.getKey(item))) Some(item) else None
  def lookup(either: Either[A,TreeNodeTrait[A,T]], key: K): Option[A] = {
    either match {
      case Left(item) =>
        lookup(item, key)
      case Right(node) =>
        if (getItemKind.compare(getItemKind.getKey(node.getItem), key) == 0) {
          val middleItem = lookupInBucket(node.getMiddle, key)
          if (middleItem.isDefined) {
            middleItem
          } else {
            val leftItem = node.getLeftNode flatMap (lookup(_, key))
            if (leftItem.isDefined) {
              leftItem
            } else {
              node.getRightNode flatMap (lookup(_, key))
            }
          }
        } else {
          None
        }
    }
  }
  def lookupInBucket(either: Either[A,TreeNodeTrait[A,T]], key: K): Option[A] = {
    either match {
      case Left(item) =>
        lookup(item, key)
      case Right(node) =>
        val middleItem = lookupInBucket(node.getMiddle, key)
        if (middleItem.isDefined) {
          middleItem
        } else {
          val leftItem = node.getLeftNode flatMap (lookupInBucket(_,key))
          if (leftItem.isDefined) {
            leftItem
          } else {
            node.getRightNode flatMap (lookupInBucket(_,key))
          }
        }
    }
  }
  def iterator: Iterator[A] = {
    new TreeNodeIterator[A,T](_rootOption)
  }
  def dump: String = {
    s"<t>${dump(getRoot map (Right(_)), Nil)}</t>"
  }
  def dump[B,T2](nodeOption: Option[Either[B,TreeNodeTrait[B,T2] with T2]], rootPath: List[AnyRef]): String = {
    nodeOption match {
      case Some(Left(item)) => s"<n>$item</n>"
      case Some(Right(node)) => dump(node, rootPath)
      case _ => "<n/>"
    }
  }
  def dump[B,T2](node: TreeNodeTrait[B,T2] with T2, rootPath: List[AnyRef]): String = {
    if (rootPath contains node) {
      s"<!-- ${node.toString} -->"
    } else {
      val subpath = node :: rootPath
      (node.getLeftNode, node.getMiddle, node.getRightNode) match {
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

trait TreeNodeTrait[+A, +T] {
  def getLeftNode: Option[Either[A,TreeNodeTrait[A,T] with T]]
  def getMiddle: Either[A,TreeNodeTrait[A,T] with T]
  def getBucket: Option[TreeNodeTrait[A,T] with T]
  def getItem: A
  def getItems = getBucket map (_.getAllItems) getOrElse (getItem :: Nil)
  def getAllItems: Iterable[A] = { getItems ++ ((getLeftNode :: getRightNode :: Nil).flatten flatMap {
    case Left(item) => item :: Nil
    case Right(tree) => tree.getAllItems
  })}
  def getRightNode: Option[Either[A,TreeNodeTrait[A,T] with T]]
  def getOrientation: Orientation = RIGHT
  def getOffset: Offset = ZERO
  def eitherToTree[B,T2](either: Either[B,TreeNodeTrait[B,T2] with T2]): Option[TreeNodeTrait[B,T2] with T2] = {
    either match {
      case Right(tree) => Some(tree)
      case _ => None
    }
  }
}

abstract sealed class AbstractTreeNode[+A,+T] extends TreeNodeTrait[A,T] {
}

// No branches
case class ItemNode[A,T](item: A) extends TreeNodeTrait[A,T] {
  override def getLeftNode = None
  override def getMiddle = Right(this.asInstanceOf[TreeNodeTrait[A,T] with T])
  override def getBucket = Some(this.asInstanceOf[TreeNodeTrait[A,T] with T])
  override def getItem = item
  override def getItems = item :: Nil
  override def getRightNode = None
}

class ShiftedNode[A,T](node: TreeNodeTrait[A,T] with T) extends TreeNodeTrait[A,T] {
  override def getOffset = ZERO
  override def getLeftNode = Some(node.getMiddle)
  override def getMiddle = node.getRightNode.get
  override def getBucket: Option[TreeNodeTrait[A,T] with T] = {
    val middle: Either[A,TreeNodeTrait[A,T] with T] = node.getRightNode.get
    eitherToTree[A,T](middle)
  }
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

abstract class SingleNode[+M,+A,+T](content: M) extends AbstractTreeNode[A,T] with SingleNodeTrait[M] {
  override def getLeftNode = None
  override def getRightNode = None

  override def getMiddle = {
    getBucket match {
      case Some(bucket) => Right(bucket)
      case _ => Left(getItem)
    }
  }
  def getMiddleRaw = content
}
trait Pair[+L,+R,+A,+T] extends TreeNodeTrait[A,T] {
  def left: L
  override def getMiddle: Either[A,TreeNodeTrait[A,T] with T] = {
    getBucket match {
      case Some(bucket) => Right(bucket)
      case _ => Left(getItem)
    }
  }
  def right: R
}
abstract class PairNode[+L,+R,+A,+T](_left: L, _right: R) extends Pair[L,R,A,T] {
  def left: L = _left
  def right: R = _right
}

trait LeftItem[+A,+T] {
  def left: A
  def getLeftNode = Some(Left(left))
}
trait LeftTree[+A,+T] {
  def left: TreeNodeTrait[A,T] with T
  def getLeftNode = Some(Right(left))
}
trait LeftNodeItem[+A,+T] {
  def right: A
  def getBucket = None
  def getItem = right
  def getRightNode = None
}
trait LeftNodeBucket[+A,+T] {
  def right: TreeNodeTrait[A,T] with T
  def getBucket = Some(right)
  def getItem = right.getItem
  def getRightNode = None
}

trait RightItem[+A,+T] {
  def right: A
  def getRightNode = Some(Left(right))
}
trait RightTree[+A,+T] {
  def right: TreeNodeTrait[A,T] with T
  def getRightNode = Some(Right(right))
}
trait RightNodeItem[+A,+T] {
  def left: A
  def getBucket = None
  def getItem = left
  def getLeftNode = None
}
trait RightNodeBucket[+A,+T] {
  def left: TreeNodeTrait[A,T] with T
  def getBucket = Some(left)
  def getItem = left.getItem
  def getLeftNode = None
}

abstract class BothNodes[+L,+M,+R,+A,+T](_left: L, _content: M, _right: R) extends PairNode[L,R,A,T](_left,_right) with SingleNodeTrait[M] {
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
trait Bucket[+A,+T] extends SingleNodeTrait[TreeNodeTrait[A,T] with T] {
  def getBucket: Option[TreeNodeTrait[A,T] with T] = Some(getMiddleRaw)
  def getItem = getMiddleRaw.getItem
}
trait Item[+A,+T] extends SingleNodeTrait[A] {
  def getBucket: Option[TreeNodeTrait[A,T] with T] = None
  def getItem = getMiddleRaw
}
