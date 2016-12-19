package org.leialearns.crystalize.immutabletree

import grizzled.slf4j.Logging

class SimpleTree[A <: AnyRef, K, V](rootOption: Option[AbstractTreeNode[A]], itemKind: ItemKind[A,K,V]) extends Tree[A, Unit](rootOption) with Logging {
  def find(key: K): Option[A] = {
    find(rootOption, key)
  }
  def find(nodeOption: Option[AbstractTreeNode[A]], key: K): Option[A] = {
    nodeOption flatMap { case node => find(node, key) }
  }
  def find(node: AbstractTreeNode[A], key: K): Option[A] = {
    val untwisted = node.untwist
    val order = itemKind.compare(key, itemKind.getKey(untwisted.getItem))
    if (order < 0) {
      find(untwisted.getLeftNode, key)
    } else if (order > 0) {
      find(untwisted.getRightNode, key)
    } else {
      lookup(untwisted, key)
    }
  }
  def lookup(nodeOption: Option[AbstractTreeNode[A]], key: K): Option[A] = {
    nodeOption flatMap { case node => lookup(node, key) }
  }
  def lookup(node: AbstractTreeNode[A], key: K): Option[A] = {
    val bucketResult = node.getBucket match {
      case ItemNode(item) =>
        val equivalent = itemKind.compare(key, itemKind.getKey(item)) == 0
        if (equivalent && itemKind.equals(key, itemKind.getKey(item))) (Some(item), false) else (None, equivalent)
      case _ => (lookup(node.getBucket.untwist, key), false)
    }
    bucketResult match {
      case (Some(item), _) => bucketResult._1
      case (_, true) =>
        val leftOption = lookup(node.getLeftNode, key)
        leftOption match {
          case Some(left) => leftOption
          case _ =>
            lookup(node.getRightNode, key)
        }
      case _ => None
    }
  }
  def iterator: Iterator[A] = {
    new TreeNodeIterator[A](rootOption)
  }
  override def insert(item: A) = {
    val newRoot = getRoot match {
      case Some(root) =>
        val key: K = itemKind.getKey(item)
        insert(item, key, root)
      case _ => createNode(None, item, None, ())
    }
    new SimpleTree[A,K,V](Some(newRoot), itemKind)
  }
  protected def insert(item: A, key: K, treeNode: AbstractTreeNode[A]): AbstractTreeNode[A] = {
    val untwisted = treeNode.untwist
    val order: Int = itemKind.compare(itemKind.getKey(item), itemKind.getKey(untwisted.getItem))
    trace(s"Compare: ${itemKind.getKey(item)} <$order> ${itemKind.getKey(untwisted.getItem)}")
    if (order < 0) {
      untwisted.getLeftNode match {
        case Some(leftNode) =>
          val newLeftNode = insert(item, key, leftNode)
          if (newLeftNode eq leftNode) treeNode else createNode(Some(newLeftNode), untwisted.getBucket, untwisted.getRightNode, ())
        case _ => createNode(Some(createNode(None, item, None, ())), untwisted.getBucket, untwisted.getRightNode, ())
      }
    } else if (order > 0) {
      untwisted.getRightNode match {
        case Some(rightNode) =>
          val newRightNode = insert(item, key, rightNode)
          if (newRightNode eq rightNode) treeNode else createNode(untwisted.getLeftNode, untwisted.getBucket, Some(newRightNode), ())
        case _ => createNode(untwisted.getLeftNode, untwisted.getBucket, Some(createNode(None, item, None, ())), ())
      }
    } else {
      val untwistedBucket = untwisted.getBucket
      val replaced = replace(item, untwistedBucket)
      val newBucket = if (replaced eq untwistedBucket) createNode(None, item, Some(replaced), ()) else replaced
      createNode(untwisted.getLeftNode, newBucket, untwisted.getRightNode, ())
    }
  }
  protected def replace(item: A, treeNodeOption: Option[AbstractTreeNode[A]]): Option[AbstractTreeNode[A]] = {
    treeNodeOption map { case treeNode => replace(item, treeNode) }
  }
  protected def replace(item: A, treeNode: AbstractTreeNode[A]): AbstractTreeNode[A] = {
    val leftNodeOption = replace(item, treeNode.getLeftNode)
    val rightNodeOption = replace(item, treeNode.getRightNode)
    val oldBucket = treeNode.getBucket
    oldBucket match {
      case ItemNode(nodeItem) =>
        if (item.equals(item, nodeItem)) {
          createNode(leftNodeOption, item, rightNodeOption, ())
        } else {
          if (differ(leftNodeOption, treeNode.getLeftNode) || differ(rightNodeOption, treeNode.getRightNode)) {
            createNode(leftNodeOption, nodeItem, rightNodeOption, ())
          } else {
            treeNode
          }
        }
      case _ =>
        val newBucket = replace(item, oldBucket)
        if (differ(leftNodeOption, treeNode.getLeftNode) || (newBucket ne oldBucket) || differ(rightNodeOption, treeNode.getRightNode)) {
          createNode(leftNodeOption, newBucket, rightNodeOption, ())
        } else {
          treeNode
        }
    }
  }
  protected def differ[X <: AnyRef](one: Option[X], other: Option[X]): Boolean = {
    (one, other) match {
      case (None, None) => false
      case (None, Some(_)) => true
      case (Some(_), None) => true
      case (Some(a), Some(b)) => a eq b
    }
  }
}
