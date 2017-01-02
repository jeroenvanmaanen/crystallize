package org.leialearns.crystallize.immutabletree

trait KeyKind[K] {
  def compare(one: K, other: K): Int
  def equals(one: K, other: K): Boolean
}
