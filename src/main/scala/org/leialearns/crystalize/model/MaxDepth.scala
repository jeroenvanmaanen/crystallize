package org.leialearns.crystalize.model

import grizzled.slf4j.Logging
import org.leialearns.crystalize._
import org.leialearns.crystalize.item.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MaxDepth extends Derived[java.lang.Long] with Propagator with Logging {
  import org.leialearns.crystalize.model.MaxDepth.MAX_DEPTH_LOCATION

  override def derive(location: DerivedLocation[java.lang.Long], state: State[_]): Future[java.lang.Long] = {
    Future {
      val oldMaxDepth: Long = state.last(MAX_DEPTH_LOCATION).getOrElse((0l,java.lang.Long.valueOf(0)))._2
      val causes = state.recompute.getOrElse(MAX_DEPTH_LOCATION, Nil)
      val result = causes map getObservedDepth match {
        case Nil => oldMaxDepth
        case nonTrivial =>
          val values = oldMaxDepth :: nonTrivial
          trace(s"Values: $values")
          values reduce (_ max _)
      }
      debug(s"Derived max depth: $oldMaxDepth: $result: $location")
      result
    }
  }

  override def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]] = {
    val observedDepth = getObservedDepth(location)
    if (observedDepth > state.last(MAX_DEPTH_LOCATION).getOrElse(0l,java.lang.Long.valueOf(0))._2) {
      MAX_DEPTH_LOCATION :: Nil
    } else {
      Nil
    }
  }

  def getObservedDepth(location: Location[_]): Long = {
    location match {
      case assigned: AssignedLocation[_] =>
        assigned.key match {
          case node: Node =>
            if (node.item.category == Observed.observedCategory) {
              node.depth - 1l
            } else {
              0l
            }
          case _ => 0l
        }
      case _ => 0l
    }
  }

  override def toString = {
    "[MaxDepth]"
  }
}

object MaxDepth {
  val MAX_DEPTH = new MaxDepth
  val MAX_DEPTH_LOCATION = new DerivedLocation[java.lang.Long](new MaxDepth, classOf[java.lang.Long])
}