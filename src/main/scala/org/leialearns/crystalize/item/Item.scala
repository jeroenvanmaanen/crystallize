package org.leialearns.crystalize.item

import org.leialearns.crystalize.Crystal

protected class Item(_category: Category, _content: Any) {
  val category = _category
  val content = _content

  override def hashCode(): Int = category.hashCode + content.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case item: Item => item.category == this.category && item.content == this.content
      case _ => false
    }
  }

  override def toString: String = {
    "[I:" + category.name + ":" + content.toString + "]"
  }
}

object Item {
  def getItem(category: Category, content: Any): Item = {
    Crystal.internalize(new Item(category, content))
  }
}
