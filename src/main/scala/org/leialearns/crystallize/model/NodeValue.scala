package org.leialearns.crystallize.model

import org.leialearns.crystallize.util.DumpCustom

sealed abstract class AbstractNodeValue(val updatedUpTo: Long, val counts: ItemCounts) extends DumpCustom {
  def isInExpectedModel: Boolean
  override def dumpAs = Iterable(updatedUpTo, counts, isInExpectedModel)
}

case class NodeValue(ordinal: Long, override val counts: ItemCounts) extends AbstractNodeValue(ordinal, counts) {
  override def isInExpectedModel = false
}

case class NodeValueInExpectedModel(ordinal: Long, override val counts: ItemCounts) extends AbstractNodeValue(ordinal, counts) {
  override def isInExpectedModel = true
}