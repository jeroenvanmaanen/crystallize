package org.leialearns.crystalize

class Location[T](key: Any, _valueType: Class[T]) {
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
