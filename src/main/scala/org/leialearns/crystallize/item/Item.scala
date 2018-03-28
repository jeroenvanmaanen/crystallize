package org.leialearns.crystallize.item

import org.leialearns.crystallize.util.{Intern, Internalizable}

class Item private (val category: Category, val content: Any) extends Internalizable {

  override def equivalenceKey: (Category, Any) = (category, content)

  override def toString: String = {
    "[I:" + category.name + ":" + content.toString + "]"
  }
}

object Item {
  def apply(category: Category, content: Any): Item = getItem(category, content)
  def getItem(category: Category, content: Any): Item = {
    Intern.internalize(new Item(category, content))
  }
}
