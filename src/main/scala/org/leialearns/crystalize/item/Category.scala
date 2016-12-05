package org.leialearns.crystalize.item

import org.leialearns.crystalize.util.{Intern, Internalizable}

class Category private (_name: String) extends Internalizable {
  val name = _name

  override def equivalenceKey = name

  override def toString: String = {
    "[C:" + this.name + "]"
  }
}

object Category {
  def getCategory(name: String): Category = {
    Intern.internalize(new Category(name))
  }
}
