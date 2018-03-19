package org.leialearns.crystallize.event

class EventHandle[State] (_ordinal: Long, _event: Event[State]) {
  val ordinal = _ordinal
  val event = _event
  var next: Option[EventHandle[State]] = None
}