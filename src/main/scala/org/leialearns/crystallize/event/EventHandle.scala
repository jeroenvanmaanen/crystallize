package org.leialearns.crystallize.event

class EventHandle[State] (val ordinal: Long, val event: Event[State]) {
  var next: Option[EventHandle[State]] = None
}