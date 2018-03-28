package org.leialearns.crystallize.interaction

import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.interaction.Actor.{empty,stop}

class NullActor(val limit: Option[Long]) extends Actor {
  def this() = {
    this(None)
  }
  var count = 0

  override def provideItem(item: Item): Unit = {
    if (limit.isDefined) {
      count += 1
    }
  }

  override def nextAction(): Item = {
    limit.map(l => {
      count += 1
      if (count > l) stop else empty
    }).getOrElse(empty)
  }
}
