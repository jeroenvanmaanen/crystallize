package org.leialearns.crystallize.immutabletree

import org.leialearns.crystallize.immutabletree.simple.Simple

class SimpleMap[K,V](rootOption: Option[Simple[(K,_)]], keyKind: KeyKind[K]) extends SimpleTree[(K,_),K](rootOption, new MapKeyExtractor[K], keyKind) with Map[K,V] {
  def this(rootOption: Option[Simple[(K,_)]]) = this(rootOption, new MapKeyKind[K] {})
  def this(keyKind: KeyKind[K]) = this(None, keyKind)
  def this() = this(None)

  def get(key: K): Option[V] = {
    (rootOption flatMap ((root) => find(Right(root), key))) map (_._2.asInstanceOf[V])
  }

  def + [V1 >: V](pair: (K, V1)): SimpleMap[K,V1] = {
    val newRoot = (rootOption map ((root) => {
      insert(pair, getKeyExtractor.extract(pair), root)
    })).getOrElse(createItemNode((), pair))
    val newRootOption = Some(newRoot)
    if (isSame(newRootOption, rootOption)) {
      this.asInstanceOf[SimpleMap[K,V1]]
    } else {
      new SimpleMap[K,V1](newRootOption)
    }
  }

  def - (key: K): SimpleMap[K,V] = {
    (rootOption flatMap ((root) => {
      val newRootOption = remove(key, root)
      if (isSame(newRootOption, rootOption)) {
        None
      } else {
        Some(new SimpleMap[K,V](newRootOption))
      }
    })).getOrElse(this)
  }

  override def iterator: Iterator[(K,V)] = {
    new TreeNodeIterator[(K,_),Simple[(K,_)]](rootOption) map ((p: (K,_)) => (p._1, p._2.asInstanceOf[V]))
  }
}
