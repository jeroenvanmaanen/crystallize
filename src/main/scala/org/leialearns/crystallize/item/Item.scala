package org.leialearns.crystallize.item

import org.leialearns.crystallize.util.{Intern, Internalizable}

class Item private (_category: Category, _content: Any) extends Internalizable {
  val category = _category
  val content = _content

  override def equivalenceKey = (category, content)

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
