package org.leialearns.crystallize.interaction

import org.leialearns.crystallize.item.{Category, Item}

trait Actor {
  def provideItem(item: Item): Unit
  def nextAction(): Item
}

object Actor {
  val actions: Category = Category.getCategory("actions")
  val empty: Item = Item.getItem(actions, "")
  val stop: Item = Item.getItem(actions, None)
}