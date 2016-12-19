package org.leialearns.crystalize.immutabletree

trait ItemKind[A,K,V] {
  def getKey(item: A): K
  def getValue(item: A): V
  def compare(one: K, other: K): Int
  def equals(one: K, other: K): Boolean
}
