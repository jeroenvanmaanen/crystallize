package org.leialearns.crystalize.item

import org.leialearns.crystalize.Crystal

protected class Node(_parent: Option[Node], _item: Item) {
  val parent = _parent
  val item = _item

  override def hashCode(): Int = this.parent.hashCode + this.item.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case node: Node => node.parent == this.parent && node.item == this.item
      case _ => false
    }
  }

  def toInnerString: String = {
    (if (parent.isDefined) parent.get.toInnerString else "") + item.toString
  }

  override def toString: String = {
    "{" + toInnerString + "}"
  }
}

object Node {
  def getNode(parent: Node, item: Item): Node = {
    Crystal.internalize(new Node(Some(parent), item))
  }

  def getNode(item: Item): Node = {
    Crystal.internalize(new Node(None, item))
  }
}