package org.leialearns.crystalize

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class State[A <: Any](_previousStateOption: Option[State[_]], _name: String, _ordinal: Long, _location: AssignedLocation[A], _valueOption: Option[A]) {
  def this(previousState: State[_], location: AssignedLocation[A], value: A) {
    this(Some(previousState), previousState.name, previousState.ordinal + 1, location, Some(value))
  }

  val previousStateOption = _previousStateOption
  val name = _name
  val ordinal = _ordinal
  val model: immutable.HashMap[Location[_],Option[Any]] = (_previousStateOption match {
    case Some(previousState) => previousState.model
    case _ => new immutable.HashMap[Location[_],Option[Any]]()
  }) + ((_location: Location[_], _valueOption: Option[Any]))
  var derived = new AtomicReference[immutable.HashMap[Location[_],(Long,Option[Any])]](new immutable.HashMap[Location[_],(Long,Option[Any])]())

  def put[T](location: AssignedLocation[T], value: T): State[T] = {
    new State(Some(this), this._name, this._ordinal + 1, location, Some(value))
  }

  def remove(location: AssignedLocation[_]): State[_] = {
    new State(Some(this), this._name, this._ordinal + 1, location, None)
  }

  def get[T](location: Location[T]): Future[T] = {
    location match {
      case DerivedLocation(key, _) =>
        val future = key.derive(this)
        future.onComplete(attempt => store(location.asInstanceOf[DerivedLocation[T]], attempt))
        future
      case AssignedLocation(key, valueType) =>
        model.get(location) flatMap ((valueOption) => valueOption map location.cast)
    }
  }

  protected def store[T](location: DerivedLocation[T], valueOption: Option[T]): Unit = {
    val oldDerived = derived.get()
    val newDerived = oldDerived + ((location, (0l, valueOption)))
    if (derived.compareAndSet(oldDerived, newDerived)) () else store(location, valueOption)
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
