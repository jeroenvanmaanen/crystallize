package org.leialearns.crystallize.immutabletree

import grizzled.slf4j.Logging
import org.leialearns.crystallize.immutabletree.simple.{Simple, SimpleCases}

class SimpleTree[A <: AnyRef, K](rootOption: Option[Simple[A]], keyExtractor: Extractor[A,K], keyKind: KeyKind[K]) extends Tree[A,Simple[A],Unit](rootOption, SimpleCases.nodeFactory[A]) with Logging {
  def getKeyExtractor = keyExtractor
  def find(key: K): Option[A] = {
    find(rootOption map (Right(_)), key)
  }
  def find(nodeOption: Option[Either[A,Simple[A]]], key: K): Option[A] = {
    nodeOption flatMap (find(_, key))
  }
  def extractKey(bucket: Simple[A]): K = {
    bucket.getMiddle match {
      case Left(item) => keyExtractor.extract(item)
      case Right(child) =>
        if (child eq bucket) throw new IllegalStateException("Non-item bucket is its own bucket") else extractKey(child)
    }
  }
  def find(either: Either[A,Simple[A]], key: K): Option[A] = {
    either match {
      case Left(item) => lookup(item, key)
      case Right(tree) => find(tree, key)
    }
  }
  def find(node: Simple[A], key: K): Option[A] = {
    val untwisted = node.untwist
    val order = keyKind.compare(key, extractKey(untwisted))
    if (order < 0) {
      find(untwisted.getLeftNode, key)
    } else if (order > 0) {
      find(untwisted.getRightNode, key)
    } else {
      lookup(Right(untwisted), key)
    }
  }
  def lookup(nodeOption: Option[Either[A,Simple[A]]], key: K): Option[A] = {
    nodeOption flatMap (lookup(_, key))
  }
  def lookup(item: A, key: K): Option[A] = if (keyKind.equals(key, keyExtractor.extract(item))) Some(item) else None
  def lookup(either: Either[A,Simple[A]], key: K): Option[A] = {
    either match {
      case Left(item) =>
        lookup(item, key)
      case Right(node) =>
        val untwisted = node.untwist
        if (keyKind.compare(keyExtractor.extract(untwisted.getItem), key) == 0) {
          val middleItem = lookupInBucket(untwisted.getMiddle, key)
          if (middleItem.isDefined) {
            middleItem
          } else {
            val leftItem = untwisted.getLeftNode flatMap (lookup(_, key))
            if (leftItem.isDefined) {
              leftItem
            } else {
              untwisted.getRightNode flatMap (lookup(_, key))
            }
          }
        } else {
          None
        }
    }
  }
  def lookupInBucket(either: Either[A,Simple[A]], key: K): Option[A] = {
    either match {
      case Left(item) =>
        lookup(item, key)
      case Right(node) =>
        val middleItem = lookupInBucket(node.getMiddle, key)
        if (middleItem.isDefined) {
          middleItem
        } else {
          val leftItem = node.getLeftNode flatMap (lookupInBucket(_,key))
          if (leftItem.isDefined) {
            leftItem
          } else {
            node.getRightNode flatMap (lookupInBucket(_,key))
          }
        }
    }
  }
  def iterator: Iterator[A] = {
    new TreeNodeIterator[A,Simple[A]](rootOption)
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
  protected def insert(item: A, key: K, treeNode: Simple[A]): Simple[A] = {
    val untwisted = treeNode.untwist
    val order: Int = keyKind.compare(keyExtractor.extract(item), extractKey(untwisted))
    trace(s"Compare: ${keyExtractor.extract(item)} <$order> ${extractKey(untwisted)}")
    if (order < 0) {
      untwisted.getLeftNode match {
        case Some(leftNode) =>
          val newLeftNode = insert(item, key, asTree(leftNode))
          if (newLeftNode eq leftNode) treeNode else createNode(Some(newLeftNode), untwisted.getMiddle, untwisted.getRightNode map asTree, ())
        case _ => createNode(Some(createNode(None, item, None, ())), untwisted.getMiddle, untwisted.getRightNode map asTree, ())
      }
    } else if (order > 0) {
      untwisted.getRightNode match {
        case Some(rightNode) =>
          val newRightNode = insert(item, key, asTree(rightNode))
          if (newRightNode eq rightNode) treeNode else createNode(untwisted.getLeftNode map asTree, untwisted.getMiddle, Some(newRightNode), ())
        case _ => createNode(untwisted.getLeftNode map asTree, untwisted.getMiddle, Some(createNode(None, item, None, ())), ())
      }
    } else {
      val untwistedMiddle = untwisted.getMiddle
      val replaced = replace(item, untwisted.getMiddle)
      trace(s"Replaced: $replaced")
      val newMiddle = if (isSame(replaced, untwistedMiddle)) Right(createNode(None, replaced, Some(createItemNode((), item)), ())) else replaced
      trace(s"New middle: $newMiddle")
      (untwisted.getLeftNode, untwisted.getRightNode) match {
        case (None, None) => asTree(newMiddle)
        case _ => createNode(untwisted.getLeftNode map asTree, newMiddle, untwisted.getRightNode map asTree, ())
      }
    }
  }
  protected def replace(newItem: A, middle: Either[A,Simple[A]]): Either[A,Simple[A]] = {
    middle match {
      case Left(item) =>
        if (keyKind.equals(keyExtractor.extract(newItem), keyExtractor.extract(item))) {
          Left(newItem)
        } else {
          Right(getNodeFactory.createNode(None, item, Some(getNodeFactory.createNode(None, newItem, None, ())), ()))
        }
      case Right(tree) =>
        Right(replace(newItem, tree))
    }
  }
  protected def replace(item: A, treeNodeOption: Option[Simple[A]]): Option[Simple[A]] = {
    treeNodeOption map (replace(item, _))
  }
  protected def replace(item: A, treeNode: Simple[A]): Simple[A] = {
    val oldLeftNodeOption = treeNode.getLeftNode map asTree
    val oldRightNodeOption = treeNode.getRightNode map asTree
    val newLeftNodeOption = replace(item, oldLeftNodeOption)
    val newRightNodeOption = replace(item, oldRightNodeOption)
    treeNode.getMiddle match {
      case Left(nodeItem) =>
        val oldKey = keyExtractor.extract(nodeItem)
        val newKey = keyExtractor.extract(item)
        if (keyKind.equals(newKey, oldKey)) {
          createNode(newLeftNodeOption, item, newRightNodeOption, ())
        } else {
          if (isSame(newLeftNodeOption, oldLeftNodeOption) && isSame(newRightNodeOption, oldRightNodeOption)) {
            treeNode
          } else {
            createNode(newLeftNodeOption, nodeItem, newRightNodeOption, ())
          }
        }
      case Right(oldBucket) =>
        val newBucket = replace(item, oldBucket)
        if (isSame(newLeftNodeOption, oldLeftNodeOption) && (newBucket eq oldBucket) && isSame(newRightNodeOption, oldRightNodeOption)) {
          treeNode
        } else {
          createNode(newLeftNodeOption, newBucket, newRightNodeOption, ())
        }
    }
  }

  def remove(key: K, node: Simple[A]): Option[Simple[A]] = {
    val untwisted = node.untwist
    val nodeKey = extractKey(untwisted)
    val order = keyKind.compare(key, nodeKey)
    if (order < 0) {
      val oldLeftNodeOption = untwisted.getLeftNode
      val newLeftNodeOption = oldLeftNodeOption flatMap ((either) => remove(key, asTree(either)))
      Some(if (isSame(oldLeftNodeOption, newLeftNodeOption)) node else createNode(newLeftNodeOption, untwisted.getMiddle, untwisted.getRightNode map asTree, ()))
    } else if (order > 0) {
      val oldRightNodeOption = untwisted.getRightNode
      val newRightNodeOption = oldRightNodeOption flatMap ((either) => remove(key, asTree(either)))
      Some(if (isSame(oldRightNodeOption, newRightNodeOption)) node else createNode(untwisted.getLeftNode map asTree, untwisted.getMiddle, newRightNodeOption, ()))
    } else {
      bucketRemove(key, node)
    }
  }

  def bucketRemove(key: K, node: Simple[A]): Option[Simple[A]] = {
    val untwisted = node.untwist
    val nodeKey = extractKey(untwisted)
    if (keyKind.equals(key, nodeKey)) {
      val leftNodeEitherOption = untwisted.getLeftNode
      val leftNodeOption = leftNodeEitherOption map asTree
      val rightNodeEitherOption = untwisted.getRightNode
      (leftNodeEitherOption, rightNodeEitherOption) match {
        case (_, None) =>
          leftNodeOption
        case (None, _) =>
          rightNodeEitherOption map asTree
        case (Some(Left(item)),_) =>
          Some(createNode(None, item, untwisted.getRightNode map asTree, ()))
        case (_,Some(rightNode)) =>
          popItem(rightNode) match {
            case (item, newRightNodeOption) =>
              Some(createNode(leftNodeOption, item, newRightNodeOption, ()))
          }
        case _ => None
      }
    } else {
      val order = keyKind.compare(key, nodeKey)
      val oldLeftNode = untwisted.getLeftNode map asTree
      val oldBucket = untwisted.getBucket.getOrElse(getNodeFactory.createNode(None, untwisted.getItem, None, ()))
      val oldRightNode = untwisted.getRightNode map asTree
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
        val untwisted = node.untwist
        val leftNodeOption = untwisted.getLeftNode map asTree
        val bucket = untwisted.getBucket.getOrElse(getNodeFactory.createNode(None, untwisted.getItem, None, ()))
        val rightNodeOption = untwisted.getRightNode map asTree
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

  protected def isSame[X <: AnyRef](aOption: Option[X], bOption: Option[X]): Boolean = {
    (aOption, bOption) match {
      case (Some(a), Some(b)) => a eq b
      case (None, None) => true
      case _ => false
    }
  }
  protected def isSame[X <: AnyRef, Y <: AnyRef](aOption: Either[X,Y], bOption: Either[X,Y]): Boolean = {
    (aOption, bOption) match {
      case (Left(a), Left(b)) => a eq b
      case (Right(a), Right(b)) => a eq b
      case _ => false
    }
  }
}
