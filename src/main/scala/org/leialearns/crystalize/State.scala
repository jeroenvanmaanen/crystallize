package org.leialearns.crystalize

import scala.collection.immutable

class State[A <: Any](_previousStateOption: Option[State[_]], _name: String, _ordinal: Long, _location: Location[A], _valueOption: Option[A]) {
  def this(previousState: State[_], location: Location[A], value: A) {
    this(Some(previousState), previousState.name, previousState.ordinal + 1, location, Some(value))
  }

  val previousStateOption = _previousStateOption
  val name = _name
  val ordinal = _ordinal
  val model: immutable.HashMap[Location[_],Option[Any]] = (_previousStateOption match {
    case Some(previousState) => previousState.model
    case None => new immutable.HashMap[Location[_],Option[Any]]()
  }) + ((_location: Location[_], _valueOption: Option[Any]))

  def put[T](location: Location[T], value: T): State[T] = {
    new State(Some(this), this._name, this._ordinal + 1, location, Some(value))
  }

  def remove(location: Location[_]): State[_] = {
    new State(Some(this), this._name, this._ordinal + 1, location, None)
  }

  def get[T](location: Location[T]): Option[Option[T]] = {
    model.get(location) map ((valueOption) => valueOption map location.valueType.cast)
  }
}
