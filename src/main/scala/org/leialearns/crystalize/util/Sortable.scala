package org.leialearns.crystalize.util

trait Sortable {
  def sortKey: String = {
    toString
  }
}
