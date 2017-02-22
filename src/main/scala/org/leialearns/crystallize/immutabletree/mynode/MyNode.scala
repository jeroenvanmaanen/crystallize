package org.leialearns.crystallize.immutabletree.mynode

import org.leialearns.crystallize.immutabletree.{Black, TreeNodeTrait}

trait MyNode[+A] extends TreeNodeTrait[A,MyNode[A]] {
  def getColor = Black()
}
