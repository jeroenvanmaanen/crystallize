package org.leialearns.crystallize.immutabletree

trait MapItemKind[K, V] extends ItemKind[(K, V), K, V] with MapKeyKind[K] {
  override def getKey(item: (K, V)): K = item._1
  override def getValue(item: (K, V)): V = item._2
}
