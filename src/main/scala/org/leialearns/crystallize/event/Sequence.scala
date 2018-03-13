package org.leialearns.crystallize.event

import scala.collection.immutable.Map
import org.leialearns.crystallize.item.Node
import org.leialearns.crystallize.model.NodeValue

class Sequence[A](_ordinal: Long) {
  val ordinal = _ordinal
  var element: Option[A] = None
}