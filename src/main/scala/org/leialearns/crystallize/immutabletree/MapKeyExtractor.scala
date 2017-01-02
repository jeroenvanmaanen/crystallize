package org.leialearns.crystallize.immutabletree

class MapKeyExtractor[K] extends Extractor[(K,_), K] {
  override def extract(pair: (K, _)): K = pair._1
}
