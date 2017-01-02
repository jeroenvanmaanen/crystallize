package org.leialearns.crystallize.util

trait Sortable {
  def sortKey: String = {
    toString
  }
}
