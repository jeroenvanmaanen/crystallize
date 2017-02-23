package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.blacknode.BlackNodeCases
import org.leialearns.crystallize.immutabletree.bucketnode.BucketNodeCases
import org.leialearns.crystallize.immutabletree.rednode.RedNodeCases

class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[RedBlackNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, RedBlackNode[A], NodeKind](rootOption, RedBlackTree.nodeFactory[A]) {
  override def insert(item: A): Tree[A, RedBlackNode[A], NodeKind] = ???
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
abstract sealed class NodeKind
case object Red extends NodeKind
case object Black extends NodeKind
case object BucketKind extends NodeKind
trait HasNodeKind {
  def getNodeKind: NodeKind
}
trait RedBlackNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with HasNodeKind
