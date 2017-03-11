package org.leialearns.crystallize.immutabletree

import java.util.NoSuchElementException

import org.leialearns.crystallize.util.LoggingConfiguration

abstract class RedBlackTreeBuilder[A,K,V](_itemKind: ItemKind[A,K,V]) extends LoggingConfiguration {
  val logger = getLogger(getClass)
  val empty = new RedBlackTree[A,K,V](None, _itemKind)

  def itemTokenLength(specification: String): Int

  def createItem(itemToken: String): A

  def createTree(specification: String): (Option[RedBlackNode[A]], String) = {
    logger.trace(s"Create tree: $specification")
    val nodeType = specification.substring(0, 1)
    val kind = nodeType.toUpperCase
    val nodeKindOption: Option[(Boolean,NodeKind)] =
      if (kind == "R") {
        Some((nodeType.charAt(0).isUpper, Red))
      } else if (kind == "B") {
        Some((nodeType.charAt(0).isUpper, Black))
      } else if (kind == "V") {
        Some((nodeType.charAt(0).isUpper, BucketKind))
      } else {
        logger.trace("None")
        None
      }
    logger.trace(s"Node kind option: $nodeKindOption")
    nodeKindOption match {
      case Some((middleIsTree, nodeKind)) =>
        createTree(specification.substring(1)) match {
          case (leftTreeOption, middleSpecification) =>
            createMiddle(middleIsTree, middleSpecification) match {
              case (treeMiddle, rightTreeSpecification) =>
                createTree(rightTreeSpecification) match {
                  case (rightTreeOption, remainder) =>
                    val result = empty.createNode(leftTreeOption, treeMiddle, rightTreeOption, nodeKind)
                    logger.trace(s"Created: ${empty.dump(result, Nil)}")
                    (Some(result), remainder)
                }
            }
        }
      case _ =>
        (None, specification.substring(1))
    }
  }

  def createMiddle(middleIsTree: Boolean, specification: String): (Either[A,TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], String) = {
    if (middleIsTree) {
      logger.trace(s"Create middle tree: $specification")
      val pair = createTree(specification)
      try {
        (Right(pair._1.get), pair._2)
      } catch {
        case _: NoSuchElementException => throw new IllegalArgumentException(s"Not a tree specification: $specification")
      }
    } else {
      logger.trace(s"Create middle item: $specification")
      val tokenLength = itemTokenLength(specification)
      val itemToken = specification.substring(0, tokenLength)
      val remainder = specification.substring(tokenLength)
      (Left(createItem(itemToken)), remainder)
    }
  }

  def dump(node: TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]): String = {
    empty.dump(node, Nil)
  }
}
