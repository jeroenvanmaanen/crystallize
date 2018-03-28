package org.leialearns.crystallize.event

import org.leialearns.crystallize.item.Item

sealed abstract class Event[State]()

case class StartEvent[State]() extends Event[State]()
case class ObservedEvent[State](node: State, item: Item) extends Event[State]()
case class ExpectedNodeAddedEvent[State](node: State, referenceOrdinal: Long) extends Event[State]()
case class ExpectedNodeRemovedEvent[State](node: State, referenceOrdinal: Long) extends Event[State]()
case class ExpectedNodeVerifiedEvent[State](node: State, referenceOrdinal: Long) extends Event[State]()
