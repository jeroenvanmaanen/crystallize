package org.leialearns.crystallize.util

import java.io.{StringWriter, Writer}

import scala.language.implicitConversions

trait Describable {
  def description: String
  def descriptionLength: Long
  def writeDescription(writer: Writer): Unit = writer.write(description)
}

trait CompositeDescribable extends Describable {

  override def description: String = {
    val writer = new StringWriter
    writeDescription(writer)
    writer.toString
  }

  override def descriptionLength: Long = {
    parts().map(_.descriptionLength).sum
  }

  override def writeDescription(writer: Writer): Unit = {
    val stream = parts()
    stream.head.writeDescription(writer)
    stream.drop(1).foreach { d => writer.append(' '); d.writeDescription(writer) }
  }

  def parts: () => Stream[Describable]
}

object Describable {

  def fromParts(traversable: Traversable[Describable]): Describable = {
    new CompositeDescribable {
      override def parts: () => Stream[Describable] = () => {
        traversable.toStream
      }
    }
  }

  implicit def toDescribable[T](n: T)(implicit prefixFree: PrefixFreeIntegral[T]): Describable = {
    new Describable {
      override def description: String = prefixFree.prefixEncode(n)
      override def descriptionLength: Long = prefixFree.descriptionLength(n)
    }
  }
}