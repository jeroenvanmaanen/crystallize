package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.simple.Simple

trait SimpleMap[K,+V] extends Map[K,V] {
  def dump: String
  def + [V1 >: V](pair: (K, V1)): SimpleMap[K,V1]
  def - (key: K): SimpleMap[K,V]
}
object SimpleMap {
  class SimpleMapImpl[K, +V](rootOption: Option[Simple[(K,V)]], _itemKind: ItemKind[(K,Any),K,Any]) extends SimpleTree[(K,Any),K,Any](rootOption, _itemKind) with SimpleMap[K,V] {

    def get(key: K): Option[V] = {
      (rootOption flatMap ((root) => find(Right(root), key))) map (_._2.asInstanceOf[V])
    }

    def + [V1 >: V](pair: (K, V1)): SimpleMapImpl[K,V1] = {
      val newRoot: Simple[(K,Any)] = (rootOption map ((root) => {
        insert(pair, getItemKind.getKey(pair), root)
      })).getOrElse(createItemNode((), pair))
      val newRootOption = Some(newRoot)
      (if (isSame(newRootOption, rootOption)) {
        this
      } else {
        newSimpleMap[K,Any](newRootOption)
      }).asInstanceOf[SimpleMapImpl[K,V1]]
    }

    def - (key: K): SimpleMapImpl[K,V] = {
      (rootOption flatMap ((root) => {
        val newRootOption: Option[Simple[(K,Any)]] = remove(key, root)
        if (isSame(newRootOption, rootOption)) {
          None
        } else {
          Some(newSimpleMap[K,Any](newRootOption))
        }
      })).getOrElse(this).asInstanceOf[SimpleMapImpl[K,V]]
    }

    override def iterator: Iterator[(K,V)] = {
      new TreeNodeIterator[(K,_),Simple[(K,_)]](rootOption) map ((p: (K,_)) => (p._1, p._2.asInstanceOf[V]))
    }
  }
  def newSimpleMap[K,V](rootOption: Option[Simple[(K,V)]], itemKind: ItemKind[(K,Any),K,Any]): SimpleMap[K,V] = {
    new SimpleMapImpl(rootOption, itemKind)
  }
  def newSimpleMap[K,V](rootOption: Option[Simple[(K,V)]]): SimpleMap[K,V] = {
    newSimpleMap(rootOption, new MapItemKind[K,Any] {})
  }
  def newSimpleMap[K](itemKind: ItemKind[(K,Any),K,Any]): SimpleMap[K,Nothing] = {
    newSimpleMap(None, itemKind)
  }
  def empty[K] = newSimpleMap[K,Nothing](None)
}