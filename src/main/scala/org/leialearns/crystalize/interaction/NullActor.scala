package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.item.Item
import org.leialearns.crystalize.interaction.Actor.empty

class NullActor extends Actor {

  override def provideItem(item: Item): Unit = {
    // Empty
  }

  override def nextAction(): Item = {
    empty
  }
}
