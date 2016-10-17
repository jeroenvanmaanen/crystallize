package org.leialearns.crystalize.model

import org.leialearns.crystalize.AssignedLocation
import org.leialearns.crystalize.item.{Node, Item, Category}

object Observed {
  val observedCategory = Category.getCategory("observed")
  val observedItem = Item.getItem(observedCategory, ())

  def createObservedLocation(node: Node) = {
    new AssignedLocation[ItemCounts](Node.getNode(node, observedItem), classOf[ItemCounts])
  }
}
