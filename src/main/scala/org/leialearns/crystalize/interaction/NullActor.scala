package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.item.Item
import org.leialearns.crystalize.interaction.Actor.{empty,stop}

class NullActor(_limit: Option[Long]) extends Actor {
  def this() = {
    this(None)
  }
  val limitOption = _limit
  var count = 0

  override def provideItem(item: Item): Unit = {
    if (limitOption.isDefined) {
      count += 1
    }
  }

  override def nextAction(): Item = {
    limitOption match {
      case Some(limit) =>
        count += 1
        if (count > limit) stop else empty
      case _ => empty
    }
  }
}
