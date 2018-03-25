package org.leialearns.crystallize.util

import java.lang.ref.Reference
import java.util.concurrent.atomic.AtomicReference

object Dump {
  def dump(prefix: String, thing: Any): Iterator[String] = {
    thing match {
      case custom: DumpCustom => dump(prefix, custom.dumpAs)
      case reference: Reference[_] => dump(prefix, Option(reference.get))
      case reference: AtomicReference[_] => dump(prefix, Option(reference.get))
      case Some(item) => dump(prefix + "! ", item)
      case None => dump(prefix, "NONE")
      case map: Map[_,_] => dumpMap(prefix, map)
      case set: Set[_] => dumpSet(prefix, set)
      case iterable: Iterable[_] => dumpIterable(prefix, iterable)
      case iterator: Iterator[_] => dumpIterator(prefix, "[[", "]]", iterator)
      case product: Product => dumpIterator(prefix, "(", ")", product.productIterator)
      case _ =>
        Array(prefix + thing.toString).iterator
    }
  }

  def dumpIterator[T](prefix: String, open: String, close: String, iterator: Iterator[T]): Iterator[String] = {
    val delimited = Seq[Any](open).iterator ++ (iterator map (Some(_))) ++ Seq[Any](close)
    val subPrefix = prefix + "  "
    delimited flatMap {
      case Some(x) => dump(subPrefix, x)
      case delimiter => Seq[String](prefix + delimiter.toString)
    }
  }

  def dumpIterable[T](prefix: String, iterable: Iterable[T]): Iterator[String] = {
    dumpIterator(prefix, "[", "]", iterable.iterator)
  }

  def dumpSet[I](prefix: String, set: Set[I]): Iterator[String] = {
    val sorted = set.toSeq sortBy sortProjection
    dumpIterator(prefix, "{", "}", sorted.iterator)
  }

  def dumpMap[K,V](prefix: String, map: Map[K,V]): Iterator[String] = {
    val sorted = map.toSeq sortBy {
      case (k, v) => sortProjection(k)
    }
    val delimited = Seq[Any]('{') ++ sorted ++ Seq[Any]('}')
    val subPrefix = prefix + "  "
    delimited.iterator flatMap {
      case (k, v) => dump(subPrefix + k.toString + ": ", v)
      case delimiter => Seq(prefix + delimiter.toString)
    }
  }

  def sortProjection(item: Any): String = {
    item match {
      case hasKey: Sortable => hasKey.sortKey
      case null => null
      case x: Any => x.toString
    }
  }
}
