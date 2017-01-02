package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging

class SimpleTree[A <: AnyRef, K](rootOption: Option[AbstractTreeNode[A]], keyExtractor: Extractor[A,K], keyKind: KeyKind[K]) extends Tree[A, Unit](rootOption) with Logging {
  def getKeyExtractor = keyExtractor
  def find(key: K): Option[A] = {
    find(rootOption, key)
  }
  def find(nodeOption: Option[AbstractTreeNode[A]], key: K): Option[A] = {
    nodeOption flatMap { case node => find(node, key) }
  }
  def find(node: AbstractTreeNode[A], key: K): Option[A] = {
    val untwisted = node.untwist
    val order = keyKind.compare(key, keyExtractor.extract(untwisted.getItem))
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
        val equivalent = keyKind.compare(key, keyExtractor.extract(item)) == 0
        if (equivalent && keyKind.equals(key, keyExtractor.extract(item))) (Some(item), false) else (None, equivalent)
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
        val key: K = keyExtractor.extract(item)
        insert(item, key, root)
      case _ => createNode(None, item, None, ())
    }
    new SimpleTree[A,K](Some(newRoot), keyExtractor, keyKind)
  }
  protected def insert(item: A, key: K, treeNode: AbstractTreeNode[A]): AbstractTreeNode[A] = {
    val untwisted = treeNode.untwist
    val order: Int = keyKind.compare(keyExtractor.extract(item), keyExtractor.extract(untwisted.getItem))
    trace(s"Compare: ${keyExtractor.extract(item)} <$order> ${keyExtractor.extract(untwisted.getItem)}")
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

  def remove(key: K, node: AbstractTreeNode[A]): Option[AbstractTreeNode[A]] = {
    val untwisted = node.untwist
    val nodeKey = keyExtractor.extract(untwisted.getItem)
    val order = keyKind.compare(key, nodeKey)
    if (order < 0) {
      val oldLeftNodeOption = untwisted.getLeftNode
      val newLeftNodeOption = oldLeftNodeOption flatMap { case leftNode => remove(key, leftNode) }
      Some(if (isSame(oldLeftNodeOption, newLeftNodeOption)) node else createNode(newLeftNodeOption, untwisted.getBucket, untwisted.getRightNode, ()))
    } else if (order > 0) {
      val oldRightNodeOption = untwisted.getRightNode
      val newRightNodeOption = oldRightNodeOption flatMap { case leftNode => remove(key, leftNode) }
      Some(if (isSame(oldRightNodeOption, newRightNodeOption)) node else createNode(untwisted.getLeftNode, untwisted.getBucket, newRightNodeOption, ()))
    } else {
      bucketRemove(key, node)
    }
  }

  def bucketRemove(key: K, node: AbstractTreeNode[A]): Option[AbstractTreeNode[A]] = {
    val untwisted = node.untwist
    val nodeKey = keyExtractor.extract(untwisted.getItem)
    if (keyKind.equals(key, nodeKey)) {
      val leftNodeOption = untwisted.getLeftNode
      val rightNodeOption = untwisted.getRightNode
      (leftNodeOption, rightNodeOption) match {
        case (_, None) =>
          leftNodeOption
        case (None, _) =>
          rightNodeOption
        case (Some(ItemNode(item)),_) =>
          Some(createNode(None, item, untwisted.getRightNode, ()))
        case (_,Some(rightNode)) =>
          popItem(rightNode) match {
            case (item, newRightNodeOption) =>
              Some(createNode(leftNodeOption, item, newRightNodeOption, ()))
          }
        case _ => None
      }
    } else {
      val order = keyKind.compare(key, nodeKey)
      val oldLeftNode = untwisted.getLeftNode
      val oldBucket = untwisted.getBucket
      val oldRightNode = untwisted.getRightNode
      if (order == 0) {
        val newLeftNode = oldLeftNode flatMap { case leftNode => bucketRemove(key, leftNode) }
        if (isSame(newLeftNode, oldLeftNode)) {
          val newBucketOption = remove(key, oldBucket)
          if (isSame(newBucketOption, Some(oldBucket))) {
            val newRightNode = oldRightNode flatMap { case rightNode => bucketRemove(key, rightNode) }
            if (isSame(newRightNode, oldRightNode)) {
              Some(node)
            } else {
              Some(createNode(oldLeftNode, oldBucket, newRightNode, ()))
            }
          } else {
            (oldLeftNode, newBucketOption, oldRightNode) match {
              case (_, Some(newBucket), _) => Some(createNode(oldLeftNode, newBucket, oldRightNode, ()))
              case (_, _, None) => oldLeftNode
              case (None, _, _) => oldRightNode
              case (_, _, Some(rightNode)) =>
                popItem(rightNode) match {
                  case (item, newRightNode) => Some(createNode(oldLeftNode, item, newRightNode, ()))
                }
            }
          }
        } else {
          Some(createNode(newLeftNode, oldBucket, oldRightNode, ()))
        }
      } else {
        Some(node)
      }
    }
  }

  def popItem(node: AbstractTreeNode[A]): (A, Option[AbstractTreeNode[A]]) = {
    node match {
      case ItemNode(item) => (item, None)
      case _ =>
        val untwisted = node.untwist
        val leftNodeOption = untwisted.getLeftNode
        val bucket = untwisted.getBucket
        val rightNodeOption = untwisted.getRightNode
        (leftNodeOption, bucket) match {
          case (Some(leftNode), _) =>
            popItem(leftNode) match {
              case (item, newLeftNode) => (item, Some(createNode(newLeftNode, bucket, rightNodeOption, ())))
            }
          case _ =>
            popItem(bucket) match {
              case (item, newBucketOption) =>
                val remainder = newBucketOption match {
                  case Some(newBucket) => Some(createNode(None, newBucket, rightNodeOption, ()))
                  case _ => rightNodeOption
                }
                (item, remainder)
            }
        }
    }
  }

  def isSame[X <: AnyRef](aOption: Option[X], bOption: Option[X]): Boolean = {
    (aOption, bOption) match {
      case (Some(a), Some(b)) => a eq b
      case (None, None) => true
      case _ => false
    }
  }
}
