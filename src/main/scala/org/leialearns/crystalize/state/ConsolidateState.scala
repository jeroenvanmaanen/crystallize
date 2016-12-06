package org.leialearns.crystalize.state

import grizzled.slf4j.Logging
import org.leialearns.crystalize.item.Category
import org.leialearns.crystalize.util.Marker
import org.leialearns.crystalize.{Location, AssignedLocation, Crystal}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable
import scala.concurrent.Future

class ConsolidateState[A <: Any](_previousStateOption: Option[State[_]], _name: String, _crystal: Crystal, _ordinal: Long, _location: AssignedLocation[A], _valueOption: Option[A]) extends State[A](None, _name, _crystal, _ordinal, _location, _valueOption) {
  var currentPreviousStateOption = _previousStateOption
  override def previousStateOption() = {
    if (currentPreviousStateOption == null) _previousStateOption else currentPreviousStateOption
  }

  def consolidate() = {
    val consolidatedDerived = consolidateRecursive(this)
    while (!updateDerived(consolidatedDerived)) {}
    currentPreviousStateOption = None
  }

  def consolidateRecursive(ancestorState: State[_]): immutable.Map[Location[_],(Long,Option[Any])] = {
    val parentDerived = ancestorState.previousStateOption() match {
      case Some(previousState) => consolidateRecursive(previousState)
      case _ => immutable.HashMap.empty
    }
    parentDerived ++ ancestorState.derived.get()
  }

  protected def updateDerived(consolidatedDerived: immutable.Map[Location[_],(Long,Option[Any])]): Boolean = {
    val oldDerived = derived.get()
    derived.compareAndSet(oldDerived, oldDerived ++ consolidatedDerived)
  }
}

object ConsolidateState extends Logging {
  val consolidateLocation = new AssignedLocation[Marker](Category.getCategory("consolidated"), classOf[Marker])
  def consolidate(crystal: Crystal): Future[Marker] = {
    info("Consolidation: start")
    val result = Future {
      var continue: Boolean = true
      do {
        val previousState = crystal.head.get()
        info(s"Consolidation try: ${previousState.ordinal}")
        val newState = new ConsolidateState[Marker](Some(previousState), "consolidated", crystal, previousState.ordinal + 1, consolidateLocation, Some(Marker.MARKER))
        if (crystal.head.compareAndSet(previousState, newState)) {
          info(s"Consolidating: [${newState.name}]: #${newState.ordinal}: ${newState.previousStateOption()}")
          newState.consolidate()
          info(s"Consolidated: [${newState.name}]: #${newState.ordinal}: ${newState.previousStateOption()}")
          continue = false
        } else {
          info(s"Consolidation retry: [${newState.name}]: #${newState.ordinal}: ${newState.previousStateOption()}")
        }
      } while (continue)
      Marker.MARKER
    }
    result onFailure {
      case throwable: Throwable => warn(s"Consolidation failed", throwable)
    }
    result
  }
}
