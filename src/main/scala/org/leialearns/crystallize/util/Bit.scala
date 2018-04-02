package org.leialearns.crystallize.util

trait Bit {
  def asInt: Int
  def asBoolean: Boolean
  def asChar: Char
}
case object ZERO extends Bit {
  def asInt = 0
  def asBoolean = false
  def asChar = 'O'
}
case object ONE extends Bit {
  def asInt = 1
  def asBoolean = true
  def asChar = 'I'
}
