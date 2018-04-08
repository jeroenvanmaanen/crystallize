package org.leialearns.crystallize.event

import org.leialearns.crystallize.item.Item

sealed abstract class Event[State] {
  def state: State
}

case class StartEvent[State]() extends Event[State] {
  def state: State = {
    throw new UnsupportedOperationException()
  }
}

case class ObservedEvent[State](state: State, item: Item) extends Event[State]
case class ParentReevaluatedEvent[State](state: State, referenceOrdinal: Long) extends Event[State]
case class ExpectedNodeAddedEvent[State](state: State, referenceOrdinal: Long) extends Event[State]
case class ExpectedNodeRemovedEvent[State](state: State, referenceOrdinal: Long) extends Event[State]
case class ExpectedNodeVerifiedEvent[State](state: State, referenceOrdinal: Long) extends Event[State]
