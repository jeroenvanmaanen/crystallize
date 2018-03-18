package org.leialearns.crystallize.util

import java.util.concurrent.atomic.AtomicReference
import grizzled.slf4j.Logging

class SimpleTree[A <: Comparable[A]](_value: A) extends DumpCustom with Logging {
  private var left: AtomicReference[SimpleTree[A]] = new AtomicReference()
  private var right: AtomicReference[SimpleTree[A]] = new AtomicReference()
  val value = _value
  def setLeft = setBranch(left)(_)
  def setRight = setBranch(right)(_)
  def setBranch(reference: AtomicReference[SimpleTree[A]])(branch: SimpleTree[A]): Unit = {
    if (!reference.compareAndSet(null, branch)) {
      throw new IllegalStateException("Left branch was already set: " + value)
    }
  }
  def add(item: A): Unit = {
    val side = value.compareTo(item);
    trace(s"Compare: ${value} - ${side} - ${item}")
    if (side < 0) {
      addTo(left)(item)
    } else if (side > 0) {
      addTo(right)(item)
    }
  }
  def addTo(reference: AtomicReference[SimpleTree[A]])(item: A): Unit = {
    if (!reference.compareAndSet(null, new SimpleTree[A](item))) {
      reference.get.add(item)
    }
  }
  override def dumpAs: Iterable[_] = {
    Iterable(Option(left.get), value, Option(right.get))
  }
}