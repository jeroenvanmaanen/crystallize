package org.leialearns.crystallize.util

import java.util.concurrent.atomic.AtomicReference
import grizzled.slf4j.Logging
import SimpleTree._

class SimpleTree[A <: Comparable[A]](_value: A) extends DumpCustom with Logging {
  private var left: AtomicReference[SimpleTree[A]] = new AtomicReference()
  private var right: AtomicReference[SimpleTree[A]] = new AtomicReference()
  val value = _value
  def getBracket[B](item: B, comparator: (A,B) => Int, withinBounds: A => Boolean): (Option[A], Option[A]) = {
    if (withinBounds(value)) {
      val side = comparator(value, item)
      if (side > 0) {
        val result = getFromReference[A,(Option[A],Option[A])](left, _.getBracket(item, comparator, withinBounds), (None, None))
        if (result._2.isEmpty) (result._1, Some(value)) else result
      } else if (side < 0) {
        val result = getFromReference[A,(Option[A],Option[A])](right, _.getBracket(item, comparator, withinBounds), (None, None))
        if (result._1.isEmpty) (Some(value), result._2) else result
      } else {
        (Some(value), Some(value))
      }
    } else {
      (None, None)
    }
  }
  def next[B](item: B, comparator: (A,B) => Int, withinBounds: A => Boolean): Option[A] = {
    if (withinBounds(value)) {
      val side = comparator(value, item)
      val result = if (side > 0) {
        trace(s"Next: ${item}: ${value}: left: ${referenceToString(left)}")
        getFromReference[A,Option[A]](left, _.next(item, comparator, withinBounds), None).orElse(Some(value))
      } else {
        getFromReference[A,Option[A]](right, _.next(item, comparator, withinBounds), None)
      }
      trace(s"Next: ${item}: ${value}: ${side}: ${result}")
      result
    } else {
      trace(s"Next: ${item}: ${value}: out of bounds")
      None
    }
  }
  def previous[B](item: B, comparator: (A,B) => Int, withinBounds: A => Boolean): Option[A] = {
    if (withinBounds(value)) {
      val side = comparator(value, item)
      val result = if (side < 0) {
        getFromReference[A,Option[A]](right, _.previous(item, comparator, withinBounds), None).orElse(Some(value))
      } else {
        getFromReference[A,Option[A]](left, _.previous(item, comparator, withinBounds), None)
      }
      trace(s"Previous: ${item}: ${value}: ${side}: ${result}")
      result
    } else {
      trace(s"Previous: ${item}: ${value}: out of bounds")
      None
    }
  }
  def add(item: A): Unit = {
    add(new SimpleTree[A](item))
  }
  def add(leaf: SimpleTree[A]): Unit = {
    val side = value.compareTo(leaf.value);
    trace(s"Compare: ${value} - ${side} - ${leaf.value}")
    if (side > 0) {
      trace(s"Add ${leaf.value} to the left of ${value}")
      addTo(left)(leaf)
    } else if (side < 0) {
      trace(s"Add ${leaf.value} to the rightt of ${value}")
      addTo(right)(leaf)
    }
  }
  override def dumpAs: Iterable[_] = {
    Iterable(Option(left.get), value, Option(right.get))
  }
}
object SimpleTree {
  def getFromReference[A <: Comparable[A],B](reference: AtomicReference[SimpleTree[A]], f: (SimpleTree[A]) => B, default: B): B = {
    Option(reference.get).map(f).getOrElse(default)
  }
  def addTo[A <: Comparable[A]](reference: AtomicReference[SimpleTree[A]])(leaf: SimpleTree[A]): Unit = {
    if (!reference.compareAndSet(null, leaf)) {
      reference.get.add(leaf)
    }
  }
  def referenceToString[A <: Comparable[A]](reference: AtomicReference[SimpleTree[A]]): String = {
    Option(reference.get).map(_.value.toString).getOrElse("{}")
  }
}