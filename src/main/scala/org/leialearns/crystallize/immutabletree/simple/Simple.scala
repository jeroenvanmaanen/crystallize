package org.leialearns.crystallize.immutabletree.simple

import org.leialearns.crystallize.immutabletree.TreeNodeTrait

trait Simple[+A] extends TreeNodeTrait[A,Simple[A]]
