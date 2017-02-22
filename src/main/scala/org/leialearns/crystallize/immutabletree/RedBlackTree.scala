package org.leialearns.crystallize.immutabletree

class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[RedBlackNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, RedBlackNode[A], NodeColor](rootOption, RedBlackTree.nodeFactory[A]) {
  override def insert(item: A): Tree[A, RedBlackNode[A], NodeColor] = ???
}
object RedBlackTree {
  def nodeFactory[A]: NodeFactory[A, RedBlackNode[A], NodeColor] = new NodeFactory[A, RedBlackNode[A], NodeColor] {
    override def createNode(leftNodeOption: Option[RedBlackNode[A]], bucket: RedBlackNode[A], rightNodeOption: Option[RedBlackNode[A]], nodeColor: NodeColor): RedBlackNode[A] = {
      ???
    }
    override def createNode(leftNodeOption: Option[RedBlackNode[A]], item: A, rightNodeOption: Option[RedBlackNode[A]], nodeColor: NodeColor): RedBlackNode[A] = {
      ???
    }
  }
}
abstract sealed class NodeColor
case class Red() extends NodeColor
case class Black() extends NodeColor
trait HasNodeColor {
  def getColor: NodeColor
}
trait RedBlackNode[A] extends TreeNodeTrait[A,RedBlackNode[A]] with HasNodeColor
trait RedNode[A] extends RedBlackNode[A] {
  override def getColor = Black()
}