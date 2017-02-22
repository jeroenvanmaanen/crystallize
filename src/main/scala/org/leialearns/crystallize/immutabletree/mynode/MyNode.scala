package org.leialearns.crystallize.immutabletree.mynode

import org.leialearns.crystallize.immutabletree.TreeNodeTrait

trait MyNode[+A] extends TreeNodeTrait[A,MyNode[A]] {
}
