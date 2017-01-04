package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

class TreeNodeIterator[A](rootOption: Option[AbstractTreeNode[A]]) extends Iterator[A] with Logging {
  var state = rootOption match {
    case Some(root) => new TreeNodeIteratorState[A] (None, root, BEFORE_LEFT)
    case _ => new TreeNodeIteratorState[A] (None, null, DONE)
  }

  override def hasNext: Boolean = {
    while (state.getParentOption.isDefined && ((state.getPosition == BEFORE_RIGHT && !state.getNode.getRightNode.isDefined) || state.getPosition == DONE)) {
      state = state.getParentOption.get
    }
    !(state.getPosition == DONE || (state.getPosition == BEFORE_RIGHT && !state.getNode.getRightNode.isDefined))
  }

  override def next(): A = {
    trace(s"Initial state: $state")
    while (!(state.getNode == null || state.getNode.isInstanceOf[ItemNode[A]])) {
      val nextStateProperties: (Option[AbstractTreeNode[A]], Position) =
        if (state.getPosition == BEFORE_LEFT) {
          (state.getNode.getLeftNode, BEFORE_BUCKET)
        } else if (state.getPosition == BEFORE_BUCKET) {
          (Some(state.getNode.getBucket), BEFORE_RIGHT)
        } else if (state.getPosition == BEFORE_RIGHT) {
          (state.getNode.getRightNode, DONE)
        } else {
          (None, DONE)
        }
      trace(s"Next state properties: $nextStateProperties")
      state = nextStateProperties match {
        case (Some(childNode), position) =>
          new TreeNodeIteratorState[A](Some(new TreeNodeIteratorState[A](state.getParentOption, state.getNode, position)), childNode.untwist, BEFORE_LEFT)
        case (None, DONE) =>
          state.getParentOption match {
            case Some(parent) => parent
            case _ => new TreeNodeIteratorState[A](None, null, DONE)
          }
        case (None, position) =>
          new TreeNodeIteratorState[A](state.getParentOption, state.getNode, position)
      }
      trace(s"Subsequent state: $state")
    }
    if (state.getNode == null) {
      throw new NoSuchElementException
    }
    val result = state.getNode.asInstanceOf[ItemNode[A]].getItem
    state = state.getParentOption match {
      case Some(parent) => parent
      case _ => new TreeNodeIteratorState[A](None, state.getNode, DONE)
    }
    result
  }
}

class TreeNodeIteratorState[A](parentOption: Option[TreeNodeIteratorState[A]], node: AbstractTreeNode[A], position: Position) {
  def getParentOption = parentOption
  def getNode = node
  def getPosition = position
  override def toString: String = s"[TreeIteratorNodeState:${getParentOption map Function.const("!")}:$getNode:$getPosition]"
}

sealed abstract class Position
case object BEFORE_LEFT extends Position
case object BEFORE_BUCKET extends Position
case object BEFORE_RIGHT extends Position
case object DONE extends Position
