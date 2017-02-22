package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.blacknode.BlackNodeCases
import org.leialearns.crystallize.immutabletree.rednode.RedNodeCases

class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[RedBlackNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, RedBlackNode[A], NodeColor](rootOption, RedBlackTree.nodeFactory[A]) {
  override def insert(item: A): Tree[A, RedBlackNode[A], NodeColor] = ???
}
object RedBlackTree {
  def nodeFactory[A]: NodeFactory[A, RedBlackNode[A], NodeColor] = new NodeFactory[A, RedBlackNode[A], NodeColor] {
    override def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], bucket: TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A], rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], nodeColor: NodeColor): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      nodeColor match {
        case Red() => RedNodeCases.nodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, ())
        case Black() => BlackNodeCases.nodeFactory.createNode(leftNodeOption, bucket, rightNodeOption, ())
      }
    }
    override def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], nodeColor: NodeColor): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      nodeColor match {
        case Red() => RedNodeCases.nodeFactory.createNode(leftNodeOption, item, rightNodeOption, ())
        case Black() => BlackNodeCases.nodeFactory.createNode(leftNodeOption, item, rightNodeOption, ())
      }
    }
  }
}
abstract sealed class NodeColor
case class Red() extends NodeColor
case class Black() extends NodeColor
trait HasNodeColor {
  def getColor: NodeColor
}
trait RedBlackNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with HasNodeColor
