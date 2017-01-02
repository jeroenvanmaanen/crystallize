package org.leialearns.crystallize.interaction

import org.leialearns.crystallize.item.{Category, Item}

trait Actor {
  def provideItem(item: Item): Unit
  def nextAction(): Item
}

object Actor {
  val actions = Category.getCategory("actions")
  val empty = Item.getItem(actions, "")
  val stop = Item.getItem(actions, None)
}