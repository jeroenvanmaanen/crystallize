package org.leialearns.crystallize.event

class Sequence[A](val ordinal: Long) {
  var element: Option[A] = None
}