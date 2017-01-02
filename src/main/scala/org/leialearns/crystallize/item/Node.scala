package org.leialearns.crystallize.item

import org.leialearns.crystallize.util.{Intern, Internalizable, Sortable}

class Node private (_parent: Option[Node], _item: Item) extends Sortable with Internalizable {
  val parent = _parent
  val item = _item

  def depth: Long = {
    parent match {
      case Some(parentNode) => parentNode.depth + 1L
      case _ => 1
    }
  }

  def equivalenceKey = (parent, item)

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
    Intern.internalize(new Node(parentOption, item))
  }

  def getNode(parent: Node, item: Item): Node = {
    Intern.internalize(new Node(Some(parent), item))
  }

  def getNode(item: Item): Node = {
    Intern.internalize(new Node(None, item))
  }
}