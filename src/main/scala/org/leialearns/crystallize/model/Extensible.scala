package org.leialearns.crystallize.model

import grizzled.slf4j.Logging
import org.leialearns.crystallize._
import org.leialearns.crystallize.item.{Category, Item, Node}
import org.leialearns.crystallize.state.State
import org.leialearns.crystallize.util.Marker

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Extensible(_node: Node) extends Derived[Marker] with Logging {
  val node: Node = _node

  override def derive(location: DerivedLocation[Marker], state: State[_]): Future[Marker] = {
    debug(s"Derive: ${state.ordinal}: $location")
    val observedLocation = Observed.createObservedLocation(node)
    val observedDistributionOption = state.get(observedLocation)
    val threshold = 10 << (node.depth - 1)
    debug(s"Threshold: $threshold ($node)")
    val future = observedDistributionOption.map((itemCounts: ItemCounts) => {
      debug(s"Item counts total: ${state.ordinal}: $location: ${itemCounts.total}")
      if (itemCounts.total < threshold) throw new NoSuchElementException else Marker.MARKER
    })
    future.onFailure({
      case t => debug(s"No observed distribution found: ${state.ordinal}: $location: $t")
    })
    future
  }

  override def hashCode(): Int = node.hashCode()

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: Extensible => other.node == node
      case _ => false
    }
  }

  override def toString = {
    "[E:" + node.toString + "]"
  }
}

object Extensible {
  val extensibleCategory = Category.getCategory("extensible")
  val extensibleItem = Item.getItem(extensibleCategory, ())

  def createExtensibleLocation(node: Node) = {
    new DerivedLocation[Marker](new Extensible(node), classOf[Marker])
  }

}