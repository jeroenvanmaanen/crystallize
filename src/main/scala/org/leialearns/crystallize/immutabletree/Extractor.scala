package org.leialearns.crystallize.immutabletree

trait Extractor[A,P] {
  def extract(a: A): P
}
