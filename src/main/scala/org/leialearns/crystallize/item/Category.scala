package org.leialearns.crystallize.item

import org.leialearns.crystallize.util.{Intern, Internalizable}

class Category private (val name: String) extends Internalizable {

  override def equivalenceKey: String = name

  override def toString: String = {
    "[C:" + this.name + "]"
  }
}

object Category {
  def apply(name: String): Category = getCategory(name)
  def getCategory(name: String): Category = {
    Intern.internalize(new Category(name))
  }
}
