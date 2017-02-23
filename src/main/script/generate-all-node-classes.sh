#!/bin/bash

SCRIPT="$(cd "$(dirname "$0")" ; pwd)"

"${SCRIPT}/generate-node-classes.sh" MyNode 'org.leialearns.crystallize.immutabletree.mynode.MyNode[A]'
"${SCRIPT}/generate-node-classes.sh" Simple 'org.leialearns.crystallize.immutabletree.simple.Simple[A]'
"${SCRIPT}/generate-node-classes.sh" RedNode 'org.leialearns.crystallize.immutabletree.RedBlackNode[A]'
"${SCRIPT}/generate-node-classes.sh" BlackNode 'org.leialearns.crystallize.immutabletree.RedBlackNode[A]'
"${SCRIPT}/generate-node-classes.sh" BucketNode 'org.leialearns.crystallize.immutabletree.RedBlackNode[A]'
