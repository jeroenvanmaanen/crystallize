package org.leialearns.crystalize.model

import org.leialearns.crystalize._
import org.leialearns.crystalize.item.Node

import scala.concurrent.Future

class MaxDepth extends Derived[Long] with Propagator {
  import MaxDepth.maxDepthLocation

  override def derive(location: DerivedLocation[Long], state: State[_]): Future[Long] = {
    Future.successful(3l)
  }

  override def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]] = {
    location match {
      case assigned: AssignedLocation[_] =>
        assigned.key match {
          case node: Node =>
            if (node.item.category == Observed.observedCategory) {
              val nodeDepth = node.depth
              if (nodeDepth > state.last(maxDepthLocation).getOrElse(0l)) {
                maxDepthLocation :: Nil
              } else {
                Nil
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

object MaxDepth {
  val maxDepthLocation = new DerivedLocation[Long](new MaxDepth, classOf[Long])
}