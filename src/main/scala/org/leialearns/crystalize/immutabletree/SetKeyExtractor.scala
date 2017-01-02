package org.leialearns.crystalize.immutabletree

class SetKeyExtractor[K] extends Extractor[K,K] {
  override def extract(item: K): K = item
}
