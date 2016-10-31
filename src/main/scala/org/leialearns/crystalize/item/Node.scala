package org.leialearns.crystalize.item

import org.leialearns.crystalize.Crystal
import org.leialearns.crystalize.util.Sortable

class Node(_parent: Option[Node], _item: Item) extends Sortable {
  val parent = _parent
  val item = _item

  override def hashCode(): Int = this.parent.hashCode + this.item.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case node: Node => node.parent == this.parent && node.item == this.item
      case _ => false
    }
  }

  def depth: Long = {
    parent match {
      case Some(parentNode) => parentNode.depth + 1L
      case _ => 1
    }
  }

  def toInnerString: String = {
    item.toString + (if (parent.isDefined) " > " + parent.get.toInnerString else "")
  }

  override def toString: String = {
    "{" + toInnerString + "}"
  }

  override def sortKey = {
    (if (parent.isDefined) parent.get.sortKey + " < " else "") + item.toString
  }
}

object Node {
  def getNode(parentOption: Option[Node], item: Item): Node = {
    Crystal.internalize(new Node(parentOption, item))
  }

  def getNode(parent: Node, item: Item): Node = {
    Crystal.internalize(new Node(Some(parent), item))
  }

  def getNode(item: Item): Node = {
    Crystal.internalize(new Node(None, item))
  }
}