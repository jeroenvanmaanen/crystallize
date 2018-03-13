package org.leialearns.crystallize.event

class EventHandle[State] (_event: Event[State]) {
  val event = _event
  var next: Option[EventHandle[State]] = None
}