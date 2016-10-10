package org.leialearns.crystalize

abstract class Location[T](_key: Any, _valueType: Class[T]) {
  val key = _key
  val valueType = _valueType

  override def equals(other: Any): Boolean = {
    if (valueType.isInstance(other)) valueType.cast(other).equals(this) else false
  }

  override def hashCode(): Int = {
    key.hashCode() + valueType.hashCode()
  }

  def cast(value: Any): T = {
    valueType.cast(value)
  }

  def cast(valueOption: Option[Any]): Option[T] = {
    valueOption map valueType.cast
  }
}

case class AssignedLocation[T](_key: Any, _valueType: Class[T]) extends Location[T](_key, _valueType)
case class DerivedLocation[T](_key: Derived[T], _valueType: Class[T]) extends Location[T](_key, _valueType)