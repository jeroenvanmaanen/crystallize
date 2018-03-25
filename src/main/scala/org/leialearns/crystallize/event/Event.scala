package org.leialearns.crystallize.event

import scala.ref.WeakReference
import org.leialearns.crystallize.item.Item

sealed abstract class Event[State]() {
}

case class StartEvent[State]() extends Event[State]()

case class ObservedEvent[State](_node: State, _item: Item) extends Event[State]() {
  val item = _item
}

case class ExpectedNodeAddedEvent[State](_node: State, _referenceOrdinal: Long) extends Event[State]()

case class ExpectedNodeRemovedEvent[State](_node: State, _referenceOrdinal: Long) extends Event[State]()

case class ExpectedNodeVerifiedEvent[State](_node: State, _referenceOrdinal: Long) extends Event[State]()
