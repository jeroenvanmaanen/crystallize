package org.leialearns.crystallize.immutabletree.rednode

import org.leialearns.crystallize.immutabletree._

case class RedNodeItem[+A](item: A) extends SingleNode[A,A,RedBlackNode[A]](item) with RedNode[A] with Item[A,RedBlackNode[A]]
case class RedNodeRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,RedBlackNode[A]](item, rightItem) with RedNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class RedNodeRightItemTree[+A](item: A, rightTree: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](item, rightTree) with RedNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class RedNodeBucket[+A](bucket: RedBlackNode[A]) extends SingleNode[RedBlackNode[A],A,RedBlackNode[A]](bucket) with RedNode[A] with Bucket[A,RedBlackNode[A]]
case class RedNodeRightBucketItem[+A](bucket: RedBlackNode[A], rightItem: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](bucket, rightItem) with RedNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class RedNodeRightBucketTree[+A](bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](bucket, rightTree) with RedNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class RedNodeLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,RedBlackNode[A]](leftItem, item) with RedNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class RedNodeItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,RedBlackNode[A]](leftItem, item, rightItem) with RedNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class RedNodeItemItemTree[+A](leftItem: A, item: A, rightTree: RedBlackNode[A])
  extends BothNodes[A,A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, item, rightTree) with RedNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class RedNodeLeftItemBucket[+A](leftItem: A, bucket: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket) with RedNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class RedNodeItemBucketItem[+A](leftItem: A, bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[A,RedBlackNode[A],A,A,RedBlackNode[A]](leftItem, bucket, rightItem) with RedNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class RedNodeItemBucketTree[+A](leftItem: A, bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[A,RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket, rightTree) with RedNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class RedNodeLeftTreeItem[+A](leftTree: RedBlackNode[A], item: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, item) with RedNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class RedNodeTreeItemItem[+A](leftTree: RedBlackNode[A], item: A, rightItem: A)
  extends BothNodes[RedBlackNode[A],A,A,A,RedBlackNode[A]](leftTree, item, rightItem) with RedNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class RedNodeTreeItemTree[+A](leftTree: RedBlackNode[A], item: A, rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],A,RedBlackNode[A],A,RedBlackNode[A]](leftTree, item, rightTree) with RedNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class RedNodeLeftTreeBucket[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket) with RedNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class RedNodeTreeBucketItem[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, bucket, rightItem) with RedNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class RedNodeTreeBucketTree[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket, rightTree) with RedNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]

// Factory object
object RedNodeCases {
  def treeToEither[A,T](tree: TreeNodeTrait[A,T] with T): Either[A,TreeNodeTrait[A,T] with T] = {
    if (tree.getLeftNode.isEmpty && tree.getRightNode.isEmpty) tree.getMiddle else Right(tree)
  }
  def nodeFactory[A]: NodeFactory[A, RedBlackNode[A], Unit] = new NodeFactory[A, RedBlackNode[A], Unit] {
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], bucket: TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A], rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], variant: Unit): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      val middle: Either[A,_] = treeToEither(bucket)
      (leftNodeOption, middle, rightNodeOption) match {
        case (None, _, None) => bucket
        case (_, Left(item), _) => createNode(leftNodeOption, item, rightNodeOption, variant)
        case _ =>
          (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
            case (None,None) => RedNodeBucket[A](bucket)
            case (None,Some(Left(rightItem))) => RedNodeRightBucketItem[A](bucket, rightItem)
            case (None,Some(Right(rightTree))) => RedNodeRightBucketTree[A](bucket, rightTree)
            case (Some(Left(leftItem)),None) => RedNodeLeftItemBucket[A](leftItem, bucket)
            case (Some(Left(leftItem)),Some(Left(rightItem))) => RedNodeItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(Left(leftItem)),Some(Right(rightTree))) => RedNodeItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(Right(leftTree)),None) => RedNodeLeftTreeBucket[A](leftTree, bucket)
            case (Some(Right(leftTree)),Some(Left(rightItem))) => RedNodeTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(Right(leftTree)),Some(Right(rightTree))) => RedNodeTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], variant: Unit): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
        case (None,None) => RedNodeItem[A](item)
        case (None,Some(Left(rightItem))) => RedNodeRightItemItem[A](item, rightItem)
        case (None,Some(Right(rightTree))) => RedNodeRightItemTree[A](item, rightTree)
        case (Some(Left(leftItem)),None) => RedNodeLeftItemItem[A](leftItem, item)
        case (Some(Left(leftItem)),Some(Left(rightItem))) => RedNodeItemItemItem[A](leftItem, item, rightItem)
        case (Some(Left(leftItem)),Some(Right(rightTree))) => RedNodeItemItemTree[A](leftItem, item, rightTree)
        case (Some(Right(leftTree)),None) => RedNodeLeftTreeItem[A](leftTree, item)
        case (Some(Right(leftTree)),Some(Left(rightItem))) => RedNodeTreeItemItem[A](leftTree, item, rightItem)
        case (Some(Right(leftTree)),Some(Right(rightTree))) => RedNodeTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
