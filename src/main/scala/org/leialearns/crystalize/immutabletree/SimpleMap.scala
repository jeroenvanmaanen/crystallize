package org.leialearns.crystalize.immutabletree

class SimpleMap[K,V](rootOption: Option[AbstractTreeNode[(K,V)]]) extends SimpleTree[(K,V),K,V](rootOption, new MapItemKind[K,V] {}) {
  def this() = this(None)

  def get(key: K): Option[V] = {
    rootOption match {
      case Some(root) => lookup(root, key) map { case entry => entry._2 }
      case _ => None
    }
  }

  def + [V1 >: V](pair: (K, V1)): SimpleMap[K,V1] = ???
}
