package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging
import org.leialearns.crystallize.immutabletree.simple.{Simple, SimpleCases}

class SimpleTree[A <: AnyRef, K, V](rootOption: Option[TreeNodeTrait[A,Simple[A]] with Simple[A]], _itemKind: ItemKind[A,K,V]) extends Tree[A,K,V,Simple[A],Unit](rootOption, SimpleCases.nodeFactory[A], _itemKind) with Logging {
  override def getBucketContext = ()
  override def insert(item: A) = {
    val newRoot = getRoot match {
      case Some(root) =>
        val key: K = getItemKind.getKey(item)
        insert(item, key, root)
      case _ => createNode(None, item, None, ())
    }
    new SimpleTree[A,K,V](Some(newRoot), getItemKind)
  }
  protected def insert(item: A, key: K, treeNode: Simple[A]): Simple[A] = {
    val order: Int = getItemKind.compare(getItemKind.getKey(item), extractKey(treeNode))
    trace(s"Compare: ${getItemKind.getKey(item)} <$order> ${extractKey(treeNode)}")
    if (order < 0) {
      treeNode.getLeftNode match {
        case Some(leftNode) =>
          val newLeftNode = insert(item, key, asTree(leftNode))
          if (newLeftNode eq leftNode) treeNode else createNode(Some(newLeftNode), treeNode.getMiddle, treeNode.getRightNode map asTree, ())
        case _ => createNode(Some(createNode(None, item, None, ())), treeNode.getMiddle, treeNode.getRightNode map asTree, ())
      }
    } else if (order > 0) {
      treeNode.getRightNode match {
        case Some(rightNode) =>
          val newRightNode = insert(item, key, asTree(rightNode))
          if (newRightNode eq rightNode) treeNode else createNode(treeNode.getLeftNode map asTree, treeNode.getMiddle, Some(newRightNode), ())
        case _ => createNode(treeNode.getLeftNode map asTree, treeNode.getMiddle, Some(createNode(None, item, None, ())), ())
      }
    } else {
      val treeNodeMiddle = treeNode.getMiddle
      val replaced = replace(item, treeNode.getMiddle)
      trace(s"Replaced: $replaced")
      val newMiddle = if (isSame(replaced, treeNodeMiddle)) Right(createNode(None, replaced, Some(createItemNode((), item)), ())) else replaced
      trace(s"New middle: $newMiddle")
      (treeNode.getLeftNode, treeNode.getRightNode) match {
        case (None, None) => asTree(newMiddle)
        case _ => createNode(treeNode.getLeftNode map asTree, newMiddle, treeNode.getRightNode map asTree, ())
      }
    }
  }

  override def remove(key: K): SimpleTree[A,K,V] = {
    val newRootOption = rootOption flatMap (remove(key,_))
    if (isSame(newRootOption, rootOption)) this else new SimpleTree[A,K,V](newRootOption, _itemKind)
  }
  def remove(key: K, node: Simple[A]): Option[TreeNodeTrait[A,Simple[A]] with Simple[A]] = {
    val nodeKey = extractKey(node)
    val order = getItemKind.compare(key, nodeKey)
    if (order < 0) {
      val oldLeftNodeOption = node.getLeftNode
      val newLeftNodeOption = oldLeftNodeOption flatMap ((either) => remove(key, asTree(either)))
      Some(if (isSame(oldLeftNodeOption, newLeftNodeOption)) node else createNode(newLeftNodeOption, node.getMiddle, node.getRightNode map asTree, ()))
    } else if (order > 0) {
      val oldRightNodeOption = node.getRightNode
      val newRightNodeOption = oldRightNodeOption flatMap ((either) => remove(key, asTree(either)))
      Some(if (isSame(oldRightNodeOption, newRightNodeOption)) node else createNode(node.getLeftNode map asTree, node.getMiddle, newRightNodeOption, ()))
    } else {
      bucketRemove(key, node)
    }
  }

  def bucketRemove(key: K, node: Simple[A]): Option[Simple[A]] = {
    val nodeKey = extractKey(node)
    if (getItemKind.equals(key, nodeKey)) {
      val leftNodeEitherOption = node.getLeftNode
      val leftNodeOption = leftNodeEitherOption map asTree
      val rightNodeEitherOption = node.getRightNode
      (leftNodeEitherOption, rightNodeEitherOption) match {
        case (_, None) =>
          leftNodeOption
        case (None, _) =>
          rightNodeEitherOption map asTree
        case (Some(Left(item)),_) =>
          Some(createNode(None, item, node.getRightNode map asTree, ()))
        case (_,Some(rightNode)) =>
          popItem(rightNode) match {
            case (item, newRightNodeOption) =>
              Some(createNode(leftNodeOption, item, newRightNodeOption, ()))
          }
        case _ => None
      }
    } else {
      val order = getItemKind.compare(key, nodeKey)
      val oldLeftNode = node.getLeftNode map asTree
      val oldBucket = node.getBucket.getOrElse(getNodeFactory.createNode(None, node.getItem, None, ()))
      val oldRightNode = node.getRightNode map asTree
      if (order == 0) {
        val newLeftNode = oldLeftNode flatMap (bucketRemove(key, _))
        if (isSame(newLeftNode, oldLeftNode)) {
          val newBucketOption = remove(key, oldBucket)
          if (isSame(newBucketOption, Some(oldBucket))) {
            val newRightNode = oldRightNode flatMap (bucketRemove(key, _))
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
  def asTree(either: Either[A,Simple[A]]): Simple[A] = getNodeFactory.asTree(either, ())
  def popItem(either: Either[A,Simple[A]]): (A, Option[Simple[A]]) = {
    either match {
      case Left(item) => (item, None)
      case Right(node) => popItem(node)
    }
  }
  def popItem(node: Simple[A]): (A, Option[Simple[A]]) = {
    (node.getLeftNode, node.getMiddle, node.getRightNode) match {
      case (None, Left(item), None) => (item, None)
      case _ =>
        val leftNodeOption = node.getLeftNode map asTree
        val bucket = node.getBucket.getOrElse(getNodeFactory.createNode(None, node.getItem, None, ()))
        val rightNodeOption = node.getRightNode map asTree
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
}
