package org.leialearns.crystalize.state

import java.util.concurrent.atomic.AtomicReference

import grizzled.slf4j.Logging
import org.leialearns.crystalize.util.{DumpCustom, OrderedKey}
import org.leialearns.crystalize.{AssignedLocation, Crystal, DerivedLocation, Location}

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class State[A <: Any](_previousStateOption: Option[State[_]], _name: String, _crystal: Crystal, _ordinal: Long, _location: AssignedLocation[A], _valueOption: Option[A]) extends Logging with DumpCustom {
  def this(previousState: State[_], location: AssignedLocation[A], value: A) {
    this(Some(previousState), previousState.name, previousState.crystal, previousState.ordinal + 1, location, Some(value))
  }

  def previousStateOption() = _previousStateOption
  val name = _name
  val crystal = _crystal
  val ordinal = _ordinal
  val model: immutable.HashMap[Location[_],Option[Any]] = (previousStateOption() match {
    case Some(previousState) => previousState.model
    case _ => new immutable.HashMap[Location[_],Option[Any]]()
  }) + ((_location: Location[_], _valueOption: Option[Any]))
  var derived = new AtomicReference[immutable.Map[Location[_],(Long,Option[Any])]](immutable.HashMap.empty)
  val recompute: Map[Location[_],immutable.List[Location[_]]] = extendRecompute(previousStateOption() match {
    case Some(previousState) => previousState.cleanRecompute()
    case _ => immutable.HashMap.empty
  }, _location :: Nil)

  private def extendRecompute(oldRecompute: Map[Location[_],immutable.List[Location[_]]], locations: Seq[Location[_]]): Map[Location[_],immutable.List[Location[_]]] = {
    val nonMembers = locations filter {case x => !oldRecompute.contains(x)}
    val nextLocations: Seq[(DerivedLocation[_],immutable.List[Location[_]])] = nonMembers flatMap {
      case nonMember => for (x <- propagateRecompute(nonMember)) yield (x, addCause(nonMember, oldRecompute.get(x)))
    }
    if (nextLocations.isEmpty) {
      oldRecompute
    } else {
      val nextRecompute = oldRecompute ++ nextLocations
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
    new State(Some(this), s"Put $location", this.crystal, this._ordinal + 1, location, Some(value))
  }

  def remove(location: AssignedLocation[_]): State[_] = {
    new State(Some(this), s"Remove $location", this.crystal, this._ordinal + 1, location, None)
  }

  def get[T](location: Location[T]): Future[T] = {
    location match {
      case derivedLocation: DerivedLocation[T] =>
        val key = derivedLocation._key
        trace(s"Get derived location with key: $ordinal: $key")
        val lastValueOption: Option[(Long,Option[T])] = if (recompute.contains(location)) fresh(derivedLocation) else last(location)
        val result = lastValueOption match {
          case Some((age, valueOption)) =>
            trace(s"Last value: ($age, $valueOption)")
            store(derivedLocation, age, valueOption map derivedLocation._valueType.cast)
            optionToFuture(valueOption)
          case _ =>
            trace(s"About to derive: $derivedLocation")
            val future = key.derive(derivedLocation, this)
            future.onComplete(attempt => {
              trace(s"Derived value for location with key: $ordinal: $key: $attempt")
              store(location.asInstanceOf[DerivedLocation[T]], 0l, attempt)
            })
            future
        }
        result
      case AssignedLocation(key, valueType) =>
        model.get(location) flatMap ((valueOption) => {
          trace(s"Retrieved assigned location with key: $ordinal: $key: $valueOption")
          valueOption map location.cast
        })
    }
  }

  def last[T](location: Location[T]): Option[(Long,Option[T])] = {
    location match {
      case DerivedLocation(key, valueType) =>
        lastDerived(location.asInstanceOf[DerivedLocation[T]])
      case AssignedLocation(key, valueType) =>
        Some(0l, model.getOrElse(location, None) map valueType.cast)
    }
  }

  def lastDerived[T](location: DerivedLocation[T]): Option[(Long,Option[T])] = {
    derived.get().get(location) match {
      case Some((age, value)) =>
        trace(s"Last derived: value: [$value]: $location")
        Some((age, value map location._valueType.cast))
      case _ =>
        var i = 0l
        var ancestorState: State[_] = this
        var result: Option[(Long,Option[T])] = None
        var greatAncestorStateOption = ancestorState.previousStateOption()
        while (result.isEmpty && greatAncestorStateOption.isDefined) {
          ancestorState = greatAncestorStateOption.get
          greatAncestorStateOption = ancestorState.previousStateOption()
          i += 1
          result = ancestorState.getDerived(location)
        }
        result match {
          case Some((age, valueOption)) => Some((age + i, valueOption))
          case _ => None
        }
    }
  }

  def getDerived[T](location: DerivedLocation[T]): Option[(Long,Option[T])] = {
    derived.get.get(location) map {
      case (age, valueOption) => (age, valueOption map location._valueType.cast)
    }
  }

  def fresh[T](location: DerivedLocation[T]): Option[(Long,Option[T])] = {
    derived.get().get(location) match {
      case Some((age, valueOption)) =>
        trace(s"Fresh: value: [$valueOption] (Age: $age): $location")
        if (age > 0l) None else Some((age, valueOption map location._valueType.cast))
      case _ => None
    }
  }

  private def increment[T](previous: (Long,T)): (Long,T) = {
    previous match {
      case (age, value) => (age + 1, value)
    }
  }

  protected def store[T](location: DerivedLocation[T], age: Long, valueOption: Option[T]): Unit = {
    val oldDerived = derived.get()
    val newDerived = oldDerived + ((location, (age, valueOption)))
    if (derived.compareAndSet(oldDerived, newDerived)) () else store(location, age, valueOption)
  }

  override def dumpAs: Iterable[_] = {
    immutable.HashMap[OrderedKey,AnyRef]() +
      (new OrderedKey("1", "type") -> "state") +
      (new OrderedKey("2", "name") -> name) +
      (new OrderedKey("3", "ordinal") -> ordinal) +
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
