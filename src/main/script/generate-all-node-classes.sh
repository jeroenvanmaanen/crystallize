#!/bin/bash

SCRIPT="$(cd "$(dirname "$0")" ; pwd)"

"${SCRIPT}/generate-node-classes.sh" MyNode 'org.leialearns.crystallize.immutabletree.mynode.MyNode[A]'
"${SCRIPT}/generate-node-classes.sh" Simple 'org.leialearns.crystallize.immutabletree.simple.Simple[A]'
