package org.leialearns.crystallize.model

import org.leialearns.crystallize._
import org.leialearns.crystallize.item.Node
import org.leialearns.crystallize.state.State

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
