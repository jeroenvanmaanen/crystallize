package org.leialearns.crystallize.expectations

import org.leialearns.crystallize.item.Item
import org.leialearns.crystallize.util.Describable._
import org.leialearns.crystallize.util.PrefixFreeIntegral.Implicits._
import org.leialearns.crystallize.util.{CompositeDescribable, Describable, Rational}

import scala.collection.immutable

class Probabilities(val map: immutable.Map[Item,(Rational,Long)]) extends CompositeDescribable {
  override def parts: () => Stream[Describable] = {
    () =>
      if (map.size > 1 && map.valuesIterator.next._2 == 0L) {
        // Uniform
        Stream()
      } else {
        map.values.toStream.map(_._2).map(toDescribable[Long])
      }
  }
}
