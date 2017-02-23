package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

class TreeNodeIterator[A,T <: TreeNodeTrait[A,T]](rootOption: Option[T]) extends Iterator[A] with Logging {
  var state: TreeNodeIteratorState[A,T] = rootOption match {
    case Some(root) => new TreeNodeIteratorInnerState[A,T] (None, root, BEFORE_LEFT)
    case _ => new TreeNodeIteratorEmptyState[A,T] ()
  }

  override def hasNext: Boolean = {
    while (state.getParentOption.isDefined && ((state.getPosition == BEFORE_RIGHT && !state.getRightNode.isDefined) || state.getPosition == DONE)) {
      state = state.getParentOption.get
    }
    !(state.getPosition == DONE || (state.getPosition == BEFORE_RIGHT && !state.getRightNode.isDefined))
  }

  override def next(): A = {
    trace(s"Initial state: $state")
    while (state.isInner) {
      state match {
        case TreeNodeIteratorInnerState(_, stateNode, statePosition) =>
          val nextStateProperties: (Option[Either[A,T]], Position) =
            if (statePosition == BEFORE_LEFT) {
              (stateNode.getLeftNode, BEFORE_BUCKET)
            } else if (statePosition == BEFORE_BUCKET) {
              (Some(stateNode.getMiddle), BEFORE_RIGHT)
            } else if (statePosition == BEFORE_RIGHT) {
              (stateNode.getRightNode, DONE)
            } else {
              (None, DONE)
            }
          trace(s"Next state properties: $nextStateProperties")
          state = nextStateProperties match {
            case (Some(Left(childItem)), position) =>
              new TreeNodeIteratorLeafState[A,T](Some(new TreeNodeIteratorInnerState[A,T](state.getParentOption, stateNode, position)), childItem, BEFORE_LEFT)
            case (Some(Right(childNode)), position) =>
              new TreeNodeIteratorInnerState[A,T](Some(new TreeNodeIteratorInnerState[A,T](state.getParentOption, stateNode, position)), childNode, BEFORE_LEFT)
            case (None, DONE) =>
              state.getParentOption match {
                case Some(parent) => parent
                case _ => new TreeNodeIteratorEmptyState[A,T]()
              }
            case (None, position) =>
              new TreeNodeIteratorInnerState[A,T](state.getParentOption, stateNode, position)
          }
          trace(s"Subsequent state: $state")
        case _ => throw new IllegalStateException("State is inner but not TreeNodeIteratorInnerState")
      }
    }
    val result =
      state match {
        case TreeNodeIteratorInnerState(_, node, _) => node.getItem
        case TreeNodeIteratorLeafState(_, item, _) => item
        case _ => throw new NoSuchElementException
      }
    state = state.getParentOption match {
      case Some(parent) => parent
      case _ => new TreeNodeIteratorEmptyState[A,T]
    }
    result
  }
}

sealed abstract class TreeNodeIteratorState[A,T <: TreeNodeTrait[A,T]](parentOption: Option[TreeNodeIteratorInnerState[A,T]], position: Position) {
  def getParentOption = parentOption
  def getPosition = position
  def getRightNode: Option[Either[A,TreeNodeTrait[A,T]]]
  def isInner: Boolean
}
case class TreeNodeIteratorInnerState[A,T <: TreeNodeTrait[A,T]](parentOption: Option[TreeNodeIteratorInnerState[A,T]], node: TreeNodeTrait[A,T], position: Position) extends TreeNodeIteratorState[A,T](parentOption, position) {
  def getNode = node
  override def getRightNode = node.getRightNode
  override def isInner = true
  override def toString: String = s"[TreeNodeIteratorInnerState:${getParentOption map Function.const("!")}:$getNode:$getPosition]"
}
case class TreeNodeIteratorLeafState[A,T <: TreeNodeTrait[A,T]](parentOption: Option[TreeNodeIteratorInnerState[A,T]], item: A, position: Position) extends TreeNodeIteratorState[A,T](parentOption, position) {
  def getItem = item
  override def getRightNode = None
  override def isInner = false
  override def toString: String = s"[TreeNodeIteratorLeafState:${getParentOption map Function.const("!")}:$getItem:$getPosition]"
}
case class TreeNodeIteratorEmptyState[A,T <: TreeNodeTrait[A,T]]() extends TreeNodeIteratorState[A,T](None, DONE) {
  def getRightNode = None
  override def isInner = false
  override def toString: String = s"[TreeNodeIteratorEmptyState:${getParentOption map Function.const("!")}:EMPTY:$getPosition]"
}

sealed abstract class Position
case object BEFORE_LEFT extends Position
case object BEFORE_BUCKET extends Position
case object BEFORE_RIGHT extends Position
case object DONE extends Position
