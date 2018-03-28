package org.leialearns.crystallize.item

import org.leialearns.crystallize.util.{Intern, Internalizable, Sortable}
import org.leialearns.crystallize.event.State
import org.leialearns.crystallize.item.Node._

class Node private (val parent: Option[Node], val item: Item) extends Sortable with Internalizable with State {
  var extensible = false

  def depth: Long = {
    parent match {
      case Some(parentNode) => parentNode.depth + 1L
      case _ => 1
    }
  }

  def equivalenceKey: (Option[Node], Item) = (parent, item)

  def toInnerString: String = {
    item.toString + (if (parent.isDefined) " > " + parent.get.toInnerString else "")
  }

  override def toString: String = {
    "{" + toInnerString + "}"
  }

  override def sortKey: String = {
    (if (parent.isDefined) parent.get.sortKey + " < " else "") + item.toString
  }

  override def impliedStates(): Iterator[Node] = parent.iterator

  override def markExtensible(): Unit = {
    extensible = true
  }

  override def isExtensible: Boolean = extensible

  def update(next: Item): Node = {
    val newParent = parent.map(p => p.update(next)).getOrElse(getNode(next))
    val result = if (newParent.isExtensible) {
      getNode(newParent, item)
    } else {
      newParent
    }
    if (result.isExtensible) {
      markExtensible()
    }
    result
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