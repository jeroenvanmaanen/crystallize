package org.leialearns.crystalize.model

import grizzled.slf4j.Logging
import org.leialearns.crystalize._
import org.leialearns.crystalize.item.{Category, Item, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Extensible(_node: Node) extends Derived[Unit] with Logging {
  val node: Node = _node

  override def derive(location: DerivedLocation[Unit], state: State[_]): Future[Unit] = {
    debug(s"Derive: ${state.ordinal}: $location")
    val observedLocation = Observed.createObservedLocation(node)
    val observedDistributionOption = state.get(observedLocation)
    val future = observedDistributionOption.map((itemCounts: ItemCounts) => {
      debug(s"Item counts total: ${state.ordinal}: $location: ${itemCounts.total}")
      if (itemCounts.total < 10) throw new NoSuchElementException else ()
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
    new DerivedLocation[Unit](new Extensible(node), classOf[Unit])
  }

}
