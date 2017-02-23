package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.blacknode.BlackNodeCases
import org.leialearns.crystallize.immutabletree.bucketnode.BucketNodeCases
import org.leialearns.crystallize.immutabletree.rednode.RedNodeCases

class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[RedBlackNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, RedBlackNode[A], NodeKind](rootOption, RedBlackTree.nodeFactory[A]) {
  override def insert(item: A): Tree[A, RedBlackNode[A], NodeKind] = {
    val (newRootOption, _) = rootOption match {
      case None => (getNodeFactory.createNode(None, item, None, Black), None)
      case Some(oldRoot) => insert(item, itemKind.getKey(item), oldRoot)
    }
    new RedBlackTree[A,K,V](Some(newRootOption), itemKind)
  }
  def insert(item: A, key: K, tree: RedBlackNode[A]): (RedBlackNode[A], Option[TreeSide]) = {
    val position = itemKind.compare(itemKind.getKey(item), itemKind.getKey(tree.getItem))
    if (position < 0) {
      (insert(item, key, tree, LeftTreeSide), Some(LeftTreeSide))
    } else if (position > 0) {
      (insert(item, key, tree, RightTreeSide), Some(RightTreeSide))
    } else {
      val rightTree = getNodeFactory.asTree(tree.getMiddle, BucketKind)
      val newBucket = getNodeFactory.createNode(None, item, Some(rightTree), BucketKind)
      (getNodeFactory.createNode(asTree(tree, LeftTreeSide), Right(newBucket), asTree(tree, RightTreeSide), tree.getNodeKind), None)
    }
  }
  def insert(item: A, key: K, tree: RedBlackNode[A], side: TreeSide): RedBlackNode[A] = {
    val pair: (RedBlackNode[A], Option[TreeSide]) =
      tree.getParent(side) match {
        case None => (getNodeFactory.createNode(None, item, None, tree.getNodeKind.getOther), None)
        case Some(Left(parentItem)) =>
          val parentTree = asTree(tree, side).get
          insert(item, key, parentTree)
        case Some(Right(parentTree)) =>
          insert(item, key, parentTree)
    }
    // TODO: Bubble up rotations to balance the tree
    (pair, side) match {
      case ((newParent, childSide), LeftTreeSide) => getNodeFactory.createNode(Some(newParent), tree.getMiddle, asTree(tree, RightTreeSide), tree.getNodeKind)
      case ((newParent, childSide), RightTreeSide) => getNodeFactory.createNode(asTree(tree, LeftTreeSide), tree.getMiddle, Some(newParent), tree.getNodeKind)
    }
  }
  def asTree[A2 <: A,T2 <: RedBlackNode[A]](parent: RedBlackNode[A], side: TreeSide): Option[RedBlackNode[A]] = {
    parent.getParent(side) map (getNodeFactory.asTree(_, parent.getNodeKind.getOther))
  }
}
object RedBlackTree {
  def nodeFactory[A]: NodeFactory[A, RedBlackNode[A], NodeKind] = new NodeFactory[A, RedBlackNode[A], NodeKind] {
    override def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], bucket: TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A], rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], nodeColor: NodeKind): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      nodeColor match {
        case Red => RedNodeCases.nodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, ())
        case Black => BlackNodeCases.nodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, ())
        case BucketKind => BucketNodeCases.nodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, ())
      }
    }
    override def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], nodeColor: NodeKind): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      nodeColor match {
        case Red => RedNodeCases.nodeFactory.createNode(leftNodeOption, item, rightNodeOption, ())
        case Black => BlackNodeCases.nodeFactory.createNode(leftNodeOption, item, rightNodeOption, ())
        case BucketKind => BucketNodeCases.nodeFactory.createNode(leftNodeOption, item, rightNodeOption, ())
      }
    }
  }
}

abstract sealed class NodeKind(_other: NodeKind) {
  def getOther = if (_other == null) BucketKind else _other
}
case object Red extends NodeKind(Black)
case object Black extends NodeKind(Red)
case object BucketKind extends NodeKind(null)

abstract sealed class TreeSide(_other: TreeSide, _leftSide: NewNodePosition, _rightSide: NewNodePosition) {
  def getOther = _other
  def getLeftSide = _leftSide
  def getRightSide = _rightSide
  def getSide(side: TreeSide): NewNodePosition = side match {
    case LeftTreeSide => _leftSide
    case RightTreeSide => _rightSide
  }
}
case object LeftTreeSide extends TreeSide(RightTreeSide, Leftmost, LeftCenter)
case object RightTreeSide extends TreeSide(LeftTreeSide, RightCenter, Rightmost)

abstract sealed class NewNodePosition(_parentSide: TreeSide, _childSide: TreeSide) {
  def getParentSide = _parentSide
  def getChildSide = _childSide
}
case object Leftmost extends NewNodePosition(LeftTreeSide, LeftTreeSide)
case object LeftCenter extends NewNodePosition(LeftTreeSide, RightTreeSide)
case object RightCenter extends NewNodePosition(RightTreeSide, LeftTreeSide)
case object Rightmost extends NewNodePosition(RightTreeSide, RightTreeSide)

trait HasNodeKind {
  def getNodeKind: NodeKind
}

trait RedBlackNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with HasNodeKind {
  def getParent(side: TreeSide): Option[Either[A,RedBlackNode[A]]] = {
    side match {
      case LeftTreeSide => getLeftNode
      case RightTreeSide => getRightNode
    }
  }
  def getNewNode(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getParent(newNodePosition.getParentSide) flatMap {
      case Left(_) => None
      case Right(parentNode) => parentNode.getParent(newNodePosition.getChildSide)
    }
  }
  def getParent(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getParent(newNodePosition.getParentSide)
  }
  def getUncle(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getParent(newNodePosition.getParentSide.getOther)
  }
}
