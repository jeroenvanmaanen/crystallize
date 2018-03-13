package org.leialearns.crystallize.model

sealed abstract class AbstractNodeValue(_counts: ItemCounts) {
  val counts = _counts
  def isInExpectedModel: Boolean
}

case class NodeValue(_counts: ItemCounts) extends AbstractNodeValue(_counts) {
  override def isInExpectedModel = false
}

case class NodeValueInExpectedModel(_counts: ItemCounts) extends AbstractNodeValue(_counts) {
  override def isInExpectedModel = true
}