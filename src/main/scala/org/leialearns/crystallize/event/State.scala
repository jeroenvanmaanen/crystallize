package org.leialearns.crystallize.event

trait State {
  def impliedStates(): Iterator[State]
  def markExtensible(): Unit
  def isExtensible(): Boolean
}