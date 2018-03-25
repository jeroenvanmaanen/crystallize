package org.leialearns.crystallize.model

import org.leialearns.crystallize.util.DumpCustom

sealed abstract class AbstractNodeValue(_updatedUpTo: Long, _counts: ItemCounts) extends DumpCustom {
  val updatedUpTo = _updatedUpTo
  val counts = _counts
  def isInExpectedModel: Boolean
  override def dumpAs = Iterable(updatedUpTo, counts, isInExpectedModel)
}

case class NodeValue(_ordinal: Long, _counts: ItemCounts) extends AbstractNodeValue(_ordinal, _counts) {
  override def isInExpectedModel = false
}

case class NodeValueInExpectedModel(_ordinal: Long, _counts: ItemCounts) extends AbstractNodeValue(_ordinal, _counts) {
  override def isInExpectedModel = true
}