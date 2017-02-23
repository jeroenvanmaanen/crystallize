package org.leialearns.crystallize.immutabletree.blacknode

import org.leialearns.crystallize.immutabletree.{RedBlackNode, Black, TreeNodeTrait}

trait BlackNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] {
  def getNodeKind = Black
}
