package org.leialearns.crystalize

import java.util.concurrent.atomic.AtomicReference

import grizzled.slf4j.Logging
import org.leialearns.crystalize.util.{OrderedKey, DumpCustom}

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class State[A <: Any](_previousStateOption: Option[State[_]], _name: String, _crystal: Crystal, _ordinal: Long, _location: AssignedLocation[A], _valueOption: Option[A]) extends Logging with DumpCustom {
  def this(previousState: State[_], location: AssignedLocation[A], value: A) {
    this(Some(previousState), previousState.name, previousState.crystal, previousState.ordinal + 1, location, Some(value))
  }

  val previousStateOption = _previousStateOption
  val name = _name
  val crystal = _crystal
  val ordinal = _ordinal
  val model: immutable.HashMap[Location[_],Option[Any]] = (_previousStateOption match {
    case Some(previousState) => previousState.model
    case _ => new immutable.HashMap[Location[_],Option[Any]]()
  }) + ((_location: Location[_], _valueOption: Option[Any]))
  var derived = new AtomicReference[immutable.HashMap[Location[_],(Long,Option[Any])]](new immutable.HashMap[Location[_],(Long,Option[Any])]())
  val recompute: Map[Location[_],immutable.List[Location[_]]] = extendRecompute(previousStateOption match {
    case Some(previousState) => previousState.cleanRecompute()
    case _ => new immutable.HashMap[Location[_],immutable.List[Location[_]]]()
  }, _location :: Nil)

  private def extendRecompute(oldRecompute: Map[Location[_],immutable.List[Location[_]]], locations: Seq[Location[_]]): Map[Location[_],immutable.List[Location[_]]] = {
    val nonMembers = locations filter {case x => !oldRecompute.contains(x)}
    val nextLocations: Seq[(DerivedLocation[_],immutable.List[Location[_]])] = nonMembers flatMap {
      case nonMember => for (x <- propagateRecompute(nonMember)) yield (x, addCause(nonMember, oldRecompute.get(x)))
    }
    val nextRecompute = oldRecompute ++ nextLocations
    if (nextLocations.isEmpty) {
      nextRecompute
    } else {
      extendRecompute(nextRecompute, nextLocations map {case (x, y) => x})
    }
  }

  private def cleanRecompute(): Map[Location[_],immutable.List[Location[_]]] = {
    val derivedSnapshot = derived.get()
    recompute filterKeys {case key => !derivedSnapshot.contains(key)}
  }

  private def addCause(location: Location[_], causeListOption: Option[immutable.List[Location[_]]]): immutable.List[Location[_]] = {
    causeListOption match {
      case Some(list) => if (list contains location) list else location :: list
      case _ => location :: Nil
    }
  }

  private def propagateRecompute(location: Location[_]): Seq[DerivedLocation[_]] = {
    crystal.propagators flatMap ((propagator) => propagator.propagate(location, this))
  }

  def put[T](location: AssignedLocation[T], value: T): State[T] = {
    new State(Some(this), this._name, this.crystal, this._ordinal + 1, location, Some(value))
  }

  def remove(location: AssignedLocation[_]): State[_] = {
    new State(Some(this), this._name, this.crystal, this._ordinal + 1, location, None)
  }

  def get[T](location: Location[T]): Future[T] = {
    location match {
      case DerivedLocation(key, _) =>
        trace(s"Get derived location with key: $ordinal: $key")
        val future = key.derive(location.asInstanceOf[DerivedLocation[T]], this)
        future.onComplete(attempt => {
          trace(s"Derived value for location with key: $ordinal: $key: $attempt")
          store(location.asInstanceOf[DerivedLocation[T]], attempt)
        })
        future
      case AssignedLocation(key, valueType) =>
        model.get(location) flatMap ((valueOption) => {
          trace(s"Retrieved assigned location with key: $ordinal: $key: $valueOption")
          valueOption map location.cast
        })
    }
  }

  def last[T](location: Location[T]): Option[T] = {
    location match {
      case DerivedLocation(key, valueType) =>
        derived.get().get(location) map valueType.cast match {
          case Some(value) => Some(value)
          case _ =>
            this.previousStateOption match {
              case Some(previousState) => previousState.last(location)
              case _ => None
            }
        }
      case AssignedLocation(key, valueType) =>
        model.getOrElse(location, None) map valueType.cast
    }
  }

  protected def store[T](location: DerivedLocation[T], valueOption: Option[T]): Unit = {
    val oldDerived = derived.get()
    val newDerived = oldDerived + ((location, (0l, valueOption)))
    if (derived.compareAndSet(oldDerived, newDerived)) () else store(location, valueOption)
  }

  override def dumpAs: Iterable[_] = {
    immutable.HashMap[OrderedKey,AnyRef]() +
      (new OrderedKey("1", "type") -> "state") +
      (new OrderedKey("2", "name") -> name) +
      (new OrderedKey("3", "ordinal") -> name) +
      (new OrderedKey("4", "model") -> model) +
      (new OrderedKey("5", "derived") -> derived.get) +
      (new OrderedKey("6", "recompute") -> recompute)
  }

  implicit def optionToFuture[T](valueOption: Option[T]): Future[T] = {
    valueOption match {
      case Some(value) => Future.successful(value)
      case None => Future.failed(new NoSuchElementException("No value"))
    }
  }
  implicit def tryToOption[T](attempt: Try[T]): Option[T] = {
    attempt match {
      case Success(value) => Some(value)
      case Failure(t) => None
    }
  }
}
