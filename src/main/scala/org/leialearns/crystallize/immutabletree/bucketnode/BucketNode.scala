package org.leialearns.crystallize.immutabletree.bucketnode

import org.leialearns.crystallize.immutabletree.{RedBlackNode, BucketKind, TreeNodeTrait}

trait BucketNode[+A] extends TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] {
  def getNodeKind = BucketKind
}
