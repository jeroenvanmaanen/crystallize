package org.leialearns.crystallize.model

import org.leialearns.crystallize.AssignedLocation
import org.leialearns.crystallize.item.{Node, Item, Category}

object Observed {
  val observedCategory = Category.getCategory("observed")
  val observedItem = Item.getItem(observedCategory, ())

  def createObservedLocation(node: Node) = {
    new AssignedLocation[ItemCounts](Node.getNode(node, observedItem), classOf[ItemCounts])
  }
}
