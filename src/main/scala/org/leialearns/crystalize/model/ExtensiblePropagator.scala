package org.leialearns.crystalize.model

import org.leialearns.crystalize._
import org.leialearns.crystalize.item.Node
import org.leialearns.crystalize.state.State

class ExtensiblePropagator extends Propagator {

  override def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]] = {
    location match {
      case assigned: AssignedLocation[_] =>
        assigned._key match {
          case node: Node =>
            if (node.item.category == Observed.observedCategory) {
              node.parent match {
                case Some(parentNode) => Extensible.createExtensibleLocation(parentNode) :: Nil
                case _ => Nil
              }
            } else {
              Nil
            }
          case _ => Nil
        }
      case _ => Nil
    }
  }
}
