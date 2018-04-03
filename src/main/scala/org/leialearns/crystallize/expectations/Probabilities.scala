package org.leialearns.crystallize.expectations

import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.util.Describable._
import org.leialearns.crystallize.util.PrefixFreeIntegral.Implicits._
import org.leialearns.crystallize.util.{CompositeDescribable, Describable, DumpCustom, Rational}

import scala.collection.immutable

class Probabilities(val map: immutable.Map[Item,(Rational,Long)]) extends CompositeDescribable with DumpCustom {
  override def parts: () => Stream[Describable] = {
    () =>
      val nonuniform = map.size > 1 && map.valuesIterator.next._2 != 0L
      Stream(booleanToDescribable(nonuniform), toDescribable(map.size)) #::: (
        if (nonuniform) {
          map.values.toStream.map(_._2).map(toDescribable[Long])
        } else {
          Stream.empty
        }
      )
  }

  override def dumpAs: Iterable[_] = Iterable(immutable.Map(
    "map" -> map,
    "description" -> description,
    "descriptionLength" -> descriptionLength
  ))
}
