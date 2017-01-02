package org.leialearns.crystallize.immutabletree

class SimpleMap[K,V](rootOption: Option[AbstractTreeNode[(K,_)]], keyKind: KeyKind[K]) extends SimpleTree[(K,_),K](rootOption, new MapKeyExtractor[K], keyKind) with Map[K,V] {
  def this(rootOption: Option[AbstractTreeNode[(K,_)]]) = this(rootOption, new MapKeyKind[K] {})
  def this(keyKind: KeyKind[K]) = this(None, keyKind)
  def this() = this(None)

  def get(key: K): Option[V] = {
    rootOption match {
      case Some(root) => lookup(root, key) map { case entry => entry._2.asInstanceOf[V] }
      case _ => None
    }
  }

  def + [V1 >: V](pair: (K, V1)): SimpleMap[K,V1] = {
    rootOption match {
      case Some(root) =>
        val newRoot = insert(pair, getKeyExtractor.extract(pair), root)
        new SimpleMap[K,V1](Some(newRoot))
      case _ =>
        new SimpleMap[K,V1](Some(createItemNode((), pair)))
    }
  }

  def - (key: K): SimpleMap[K,V] = {
    rootOption match {
      case Some(root) =>
        val newRootOption = remove(key, root)
        if (isSame(newRootOption, rootOption)) {
          this
        } else {
          new SimpleMap[K,V](newRootOption)
        }
      case _ => this
    }
  }

  override def iterator: Iterator[(K,V)] = {
    new TreeNodeIterator(rootOption) map { case (k: Any, v: Any) => (k.asInstanceOf[K], v.asInstanceOf[V]) }
  }
}
