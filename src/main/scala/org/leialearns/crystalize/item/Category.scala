package org.leialearns.crystalize.item

import org.leialearns.crystalize.Crystal

protected class Category(_name: String) {
  val name = _name

  override def hashCode(): Int = this.name.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case category: Category => category.name == this.name
      case _ => false
    }
  }

  override def toString: String = {
    "[C:" + this.name + "]"
  }
}

object Category {
  def getCategory(name: String): Category = {
    Crystal.internalize(new Category(name))
  }
}
