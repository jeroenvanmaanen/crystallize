package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.blacknode.BlackNodeCases
import org.leialearns.crystallize.immutabletree.bucketnode.BucketNodeCases
import org.leialearns.crystallize.immutabletree.rednode.RedNodeCases

class RedBlackTree[A, K, V](rootOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], _itemKind: ItemKind[A,K,V]) extends Tree[A,K,V,RedBlackNode[A],NodeKind](rootOption, RedBlackTree.nodeFactory[A], _itemKind) {
  override def insert(item: A): RedBlackTree[A, K, V] = {
    val (newRootOption, _) = rootOption match {
      case None => (getNodeFactory.createNode(None, item, None, Black), None)
      case Some(oldRoot) =>
        insert(item, getItemKind.getKey(item), oldRoot) match {
          case (newRoot, side) => (if (newRoot.getNodeKind == Red) changeKind(newRoot, Black) else newRoot, side)
        }
    }
    new RedBlackTree[A,K,V](Some(newRootOption), getItemKind)
  }
  def insert(item: A, key: K, tree: RedBlackNode[A]): (RedBlackNode[A], Option[TreeSide]) = {
    val position = getItemKind.compare(getItemKind.getKey(item), getItemKind.getKey(tree.getItem))
    if (position < 0) {
      if (tree.getNodeKind == BucketKind) {
        val leftTree = getNodeFactory.createNode(None, item, None, BucketKind)
        (getNodeFactory.createNode(Some(leftTree), tree, None, Red), Some(LeftTreeSide))
      } else {
        (insert(item, key, tree, LeftTreeSide), Some(LeftTreeSide))
      }
    } else if (position > 0) {
      if (tree.getNodeKind == BucketKind) {
        val rightTree = getNodeFactory.createNode(None, item, None, BucketKind)
        (getNodeFactory.createNode(None, tree, Some(rightTree), Red), Some(RightTreeSide))
      } else {
        (insert(item, key, tree, RightTreeSide), Some(RightTreeSide))
      }
    } else {
      val rightTree = getNodeFactory.asTree(tree.getMiddle, BucketKind)
      val newBucket = getNodeFactory.createNode(None, item, Some(rightTree), BucketKind)
      (getNodeFactory.createNode(asTree(tree, LeftTreeSide), Right(newBucket), asTree(tree, RightTreeSide), tree.getNodeKind), None)
    }
  }
  def insert(item: A, key: K, tree: RedBlackNode[A], side: TreeSide): RedBlackNode[A] = {
    (tree.getChild(side) match {
      case None => (getNodeFactory.createNode(None, item, None, BucketKind), None)
      case Some(Left(parentItem)) =>
        val parentTree = asTree(tree, side).get
        insert(item, key, parentTree)
      case Some(Right(parentTree)) =>
        insert(item, key, parentTree)
    }) match {
      case (newParent, childSideOption) =>
        val coloredParent = if (newParent.getNodeKind == BucketKind) changeKind(newParent, Red) else newParent
        getSideOfNewDoubleRedEdgeNode(coloredParent, childSideOption) match {
          case Some(childSide) =>
            val alignedParent = if (childSide == side) coloredParent else swap(coloredParent, childSide, Red, Red)

            val uncle = asTree(tree, side.getOther)
            val uncleKind = (uncle map {
              case uncleTree => uncleTree.getNodeKind
            }).getOrElse(Black)
            if (uncleKind == Red) {
              val blackParent = changeKind(alignedParent, Black)
              val blackUncleOption = uncle map (changeKind(_, Black))
              changeKind(replaceSide(Some(blackParent), replaceSide(blackUncleOption, tree, side.getOther), side), Red)
            } else {
              val alignedGrandparent = replaceSide(Some(alignedParent), tree, side)
              swap(alignedGrandparent, side, Black, Red)
            }
          case _ =>
            replaceSide(Some(coloredParent), tree, side)
        }
    }
  }
  def getSideOfNewDoubleRedEdgeNode(parent: RedBlackNode[A], sideOption: Option[TreeSide]): Option[TreeSide] = {
    (parent.getNodeKind match {
      case Red =>
        sideOption
      case _ =>
        None
    }) flatMap {
      case side =>
        parent.getChild(side) flatMap {
          case Left(_) => None
          case Right(child) => if (child.getNodeKind == Red) Some(side) else None
        }
    }
  }
  def replaceSide(newChild: Option[RedBlackNode[A]], tree: RedBlackNode[A], side: TreeSide): RedBlackNode[A] = {
    side match {
      case LeftTreeSide => getNodeFactory.createNode(newChild, tree.getMiddle, asTree(tree, RightTreeSide), tree.getNodeKind)
      case RightTreeSide => getNodeFactory.createNode(asTree(tree, LeftTreeSide), tree.getMiddle, newChild, tree.getNodeKind)
    }
  }
  def swap(parent: RedBlackNode[A], side: TreeSide, newParentKind: NodeKind, newChildKind: NodeKind) = {
    val child = parent.getChild(side).get
    val childTreeOption = child match {
      case Left(_) => None
      case Right(childTree) => Some(childTree)
    }
    val childMiddle = child.right flatMap (_.getMiddle)
    val sameSideChild = childTreeOption flatMap (asTree(_, side))
    val centerChild = childTreeOption flatMap (asTree(_, side.getOther))
    val otherSide = asTree(parent, side.getOther)
    side match {
      case LeftTreeSide =>
        val newChild = getNodeFactory.createNode(centerChild, parent.getMiddle, otherSide, newChildKind)
        getNodeFactory.createNode(sameSideChild, childMiddle, Some(newChild), newParentKind)
      case RightTreeSide =>
        val newChild = getNodeFactory.createNode(otherSide, parent.getMiddle, centerChild, newChildKind)
        getNodeFactory.createNode(Some(newChild), childMiddle, sameSideChild, newParentKind)
    }
  }
  def changeKind(node: RedBlackNode[A], kind: NodeKind): RedBlackNode[A] = {
    if (kind == node.getNodeKind) {
      node
    } else {
      getNodeFactory.createNode(asTree(node, LeftTreeSide), node.getMiddle, asTree(node, RightTreeSide), kind)
    }
  }
  def asTree[A2 <: A,T2 <: RedBlackNode[A]](parent: RedBlackNode[A], side: TreeSide): Option[RedBlackNode[A]] = {
    val result = parent.getChild(side) map (getNodeFactory.asTree(_, BucketKind))
    trace(s"As tree: $result")
    result
  }
  def verifyBalance(): Unit = {
    val blackDepth = (rootOption map (_.verifyBlackDepth())).getOrElse(0)
    debug(s"Verified black depth: $blackDepth")
  }
  override def showVariant[B,T2 <: RedBlackNode[A]](node: TreeNodeTrait[B,T2] with T2): Option[String] = {
    node.getNodeKind match {
      case Red => Some("R")
      case Black => Some("B")
      case _ => None
    }
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
  def getChild(side: TreeSide): Option[Either[A,RedBlackNode[A]]] = {
    side match {
      case LeftTreeSide => getLeftNode
      case RightTreeSide => getRightNode
      case _ => None
    }
  }
  def getNewNode(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getChild(newNodePosition.getParentSide) flatMap {
      case Left(_) => None
      case Right(parentNode) => parentNode.getChild(newNodePosition.getChildSide)
    }
  }
  def getNewNodeParent(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getChild(newNodePosition.getParentSide)
  }
  def getNewNodeUncle(newNodePosition: NewNodePosition): Option[Either[A,RedBlackNode[A]]] = {
    getChild(newNodePosition.getParentSide.getOther)
  }
  def verifyBlackDepth(): Int = {
    val getBranchDepth: (Either[A,RedBlackNode[A]]) => Int = {
      case Left(item) => 0
      case Right(tree) => tree.verifyBlackDepth()
    }
    val leftDepth = (getLeftNode map getBranchDepth).getOrElse(0)
    val rightDepth = (getLeftNode map getBranchDepth).getOrElse(0)
    if (leftDepth != rightDepth) {
      throw new IllegalStateException(s"Unbalanced tree: $leftDepth: $rightDepth: $this")
    }
    leftDepth
  }
}
