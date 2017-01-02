package org.leialearns.crystallize

import org.leialearns.crystallize.util.Sortable

abstract class Location[T](_char: Char, _key: Any, _valueType: Class[T]) extends Sortable {
  if (_valueType == java.lang.Void.TYPE) throw new IllegalArgumentException(s"Cannot use location with void type: ${_valueType}: ${_char}: ${_key}")
  val char = _char
  val key = _key
  val valueType = _valueType

  override def equals(other: Any): Boolean = {
    other match {
      case otherLocation: Location[_] =>
        otherLocation.char == char && otherLocation.key == key && otherLocation.valueType == valueType
      case _ => false
    }
  }

  override def hashCode(): Int = {
    char.hashCode() + key.hashCode() + valueType.hashCode()
  }

  override def sortKey = {
    key match {
      case sortable: Sortable => s"${sortable.sortKey} $char"
      case _ => s"${key.toString} $char"
    }
  }

  override def toString = {
    s"[${char}L:$key:${valueType.getSimpleName}]"
  }

  def cast(value: Any): T = {
    valueType.cast(value)
  }

  def cast(valueOption: Option[Any]): Option[T] = {
    valueOption map valueType.cast
  }
}

case class AssignedLocation[T](_key: Any, _valueType: Class[T]) extends Location[T]('A', _key, _valueType)
case class DerivedLocation[T](_key: Derived[T], _valueType: Class[T]) extends Location[T]('D', _key, _valueType)