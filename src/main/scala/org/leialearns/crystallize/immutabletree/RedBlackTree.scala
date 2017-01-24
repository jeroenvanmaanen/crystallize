package org.leialearns.crystallize.immutabletree

//class RedBlackTree[A, K <: Ordered[K], V](rootOption: Option[MyNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, NodeColor, MyNode[A]](rootOption, MyNodeCases.nodeFactory[A]) {
//  override def insert(item: A): Tree[A, NodeColor, MyNode[A]] = ???
//}
abstract sealed class NodeColor
case class Red() extends NodeColor
case class Black() extends NodeColor
trait RedBlackNode[A] extends TreeNodeTrait[A,RedBlackNode[A]] {
  def getColor: NodeColor
}
trait RedNode[A] extends RedBlackNode[A] {
  override def getColor = Black()
}