package org.leialearns.crystallize.immutabletree.rednode

import org.leialearns.crystallize.immutabletree.{RedBlackNode, Red, TreeNodeTrait}

trait RedNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] {
  def getNodeKind = Red
}
