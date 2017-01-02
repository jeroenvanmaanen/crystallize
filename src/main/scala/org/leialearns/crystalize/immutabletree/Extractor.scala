package org.leialearns.crystalize.immutabletree

trait Extractor[A,P] {
  def extract(a: A): P
}
