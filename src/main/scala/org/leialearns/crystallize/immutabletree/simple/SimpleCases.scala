package org.leialearns.crystallize.immutabletree.simple

import org.leialearns.crystallize.immutabletree._

case class SimpleItem[+A](item: A) extends SingleNode[A,A,Simple[A]](item) with Simple[A] with Item[A,Simple[A]]
case class SimpleRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,Simple[A]](item, rightItem) with Simple[A]
  with RightItem[A,Simple[A]]
  with RightNodeItem[A,Simple[A]]
case class SimpleRightItemTree[+A](item: A, rightTree: Simple[A])
  extends PairNode[A,Simple[A],A,Simple[A]](item, rightTree) with Simple[A]
  with RightTree[A,Simple[A]]
  with RightNodeItem[A,Simple[A]]
case class SimpleBucket[+A](bucket: Simple[A]) extends SingleNode[Simple[A],A,Simple[A]](bucket) with Simple[A] with Bucket[A,Simple[A]]
case class SimpleRightBucketItem[+A](bucket: Simple[A], rightItem: A)
  extends PairNode[Simple[A],A,A,Simple[A]](bucket, rightItem) with Simple[A]
  with RightItem[A,Simple[A]]
  with RightNodeBucket[A,Simple[A]]
case class SimpleRightBucketTree[+A](bucket: Simple[A], rightTree: Simple[A])
  extends PairNode[Simple[A],Simple[A],A,Simple[A]](bucket, rightTree) with Simple[A]
  with RightTree[A,Simple[A]]
  with RightNodeBucket[A,Simple[A]]
case class SimpleLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,Simple[A]](leftItem, item) with Simple[A]
  with LeftNodeItem[A,Simple[A]]
  with LeftItem[A,Simple[A]]
case class SimpleItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,Simple[A]](leftItem, item, rightItem) with Simple[A]
  with LeftItem[A,Simple[A]]
  with Item[A,Simple[A]]
  with RightItem[A,Simple[A]]
case class SimpleItemItemTree[+A](leftItem: A, item: A, rightTree: Simple[A])
  extends BothNodes[A,A,Simple[A],A,Simple[A]](leftItem, item, rightTree) with Simple[A]
  with LeftItem[A,Simple[A]]
  with Item[A,Simple[A]]
  with RightTree[A,Simple[A]]
case class SimpleLeftItemBucket[+A](leftItem: A, bucket: Simple[A])
  extends PairNode[A,Simple[A],A,Simple[A]](leftItem, bucket) with Simple[A]
  with LeftNodeBucket[A,Simple[A]]
  with LeftItem[A,Simple[A]]
case class SimpleItemBucketItem[+A](leftItem: A, bucket: Simple[A], rightItem: A)
  extends BothNodes[A,Simple[A],A,A,Simple[A]](leftItem, bucket, rightItem) with Simple[A]
  with LeftItem[A,Simple[A]]
  with Bucket[A,Simple[A]]
  with RightItem[A,Simple[A]]
case class SimpleItemBucketTree[+A](leftItem: A, bucket: Simple[A], rightTree: Simple[A])
  extends BothNodes[A,Simple[A],Simple[A],A,Simple[A]](leftItem, bucket, rightTree) with Simple[A]
  with LeftItem[A,Simple[A]]
  with Bucket[A,Simple[A]]
  with RightTree[A,Simple[A]]
case class SimpleLeftTreeItem[+A](leftTree: Simple[A], item: A)
  extends PairNode[Simple[A],A,A,Simple[A]](leftTree, item) with Simple[A]
  with LeftNodeItem[A,Simple[A]]
  with LeftTree[A,Simple[A]]
case class SimpleTreeItemItem[+A](leftTree: Simple[A], item: A, rightItem: A)
  extends BothNodes[Simple[A],A,A,A,Simple[A]](leftTree, item, rightItem) with Simple[A]
  with LeftTree[A,Simple[A]]
  with Item[A,Simple[A]]
  with RightItem[A,Simple[A]]
case class SimpleTreeItemTree[+A](leftTree: Simple[A], item: A, rightTree: Simple[A])
  extends BothNodes[Simple[A],A,Simple[A],A,Simple[A]](leftTree, item, rightTree) with Simple[A]
  with LeftTree[A,Simple[A]]
  with Item[A,Simple[A]]
  with RightTree[A,Simple[A]]
case class SimpleLeftTreeBucket[+A](leftTree: Simple[A], bucket: Simple[A])
  extends PairNode[Simple[A],Simple[A],A,Simple[A]](leftTree, bucket) with Simple[A]
  with LeftNodeBucket[A,Simple[A]]
  with LeftTree[A,Simple[A]]
case class SimpleTreeBucketItem[+A](leftTree: Simple[A], bucket: Simple[A], rightItem: A)
  extends BothNodes[Simple[A],Simple[A],A,A,Simple[A]](leftTree, bucket, rightItem) with Simple[A]
  with LeftTree[A,Simple[A]]
  with Bucket[A,Simple[A]]
  with RightItem[A,Simple[A]]
case class SimpleTreeBucketTree[+A](leftTree: Simple[A], bucket: Simple[A], rightTree: Simple[A])
  extends BothNodes[Simple[A],Simple[A],Simple[A],A,Simple[A]](leftTree, bucket, rightTree) with Simple[A]
  with LeftTree[A,Simple[A]]
  with Bucket[A,Simple[A]]
  with RightTree[A,Simple[A]]

// Factory object
object SimpleCases {
  def treeToEither[A,T](tree: TreeNodeTrait[A,T] with T): Either[A,TreeNodeTrait[A,T] with T] = {
    if (tree.getLeftNode.isEmpty && tree.getRightNode.isEmpty) tree.getMiddle else Right(tree)
  }
  def nodeFactory[A]: NodeFactory[A, Simple[A], Unit] = new NodeFactory[A, Simple[A], Unit] {
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,Simple[A]] with Simple[A]], bucket: TreeNodeTrait[A,Simple[A]] with Simple[A], rightNodeOption: Option[TreeNodeTrait[A,Simple[A]] with Simple[A]], variant: Unit): TreeNodeTrait[A,Simple[A]] with Simple[A] = {
      val middle: Either[A,_] = treeToEither(bucket)
      (leftNodeOption, middle, rightNodeOption) match {
        case (None, _, None) => bucket
        case (_, Left(item), _) => createNode(leftNodeOption, item, rightNodeOption, variant)
        case _ =>
          (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
            case (None,None) => SimpleBucket[A](bucket)
            case (None,Some(Left(rightItem))) => SimpleRightBucketItem[A](bucket, rightItem)
            case (None,Some(Right(rightTree))) => SimpleRightBucketTree[A](bucket, rightTree)
            case (Some(Left(leftItem)),None) => SimpleLeftItemBucket[A](leftItem, bucket)
            case (Some(Left(leftItem)),Some(Left(rightItem))) => SimpleItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(Left(leftItem)),Some(Right(rightTree))) => SimpleItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(Right(leftTree)),None) => SimpleLeftTreeBucket[A](leftTree, bucket)
            case (Some(Right(leftTree)),Some(Left(rightItem))) => SimpleTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(Right(leftTree)),Some(Right(rightTree))) => SimpleTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,Simple[A]] with Simple[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,Simple[A]] with Simple[A]], variant: Unit): TreeNodeTrait[A,Simple[A]] with Simple[A] = {
      (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
        case (None,None) => SimpleItem[A](item)
        case (None,Some(Left(rightItem))) => SimpleRightItemItem[A](item, rightItem)
        case (None,Some(Right(rightTree))) => SimpleRightItemTree[A](item, rightTree)
        case (Some(Left(leftItem)),None) => SimpleLeftItemItem[A](leftItem, item)
        case (Some(Left(leftItem)),Some(Left(rightItem))) => SimpleItemItemItem[A](leftItem, item, rightItem)
        case (Some(Left(leftItem)),Some(Right(rightTree))) => SimpleItemItemTree[A](leftItem, item, rightTree)
        case (Some(Right(leftTree)),None) => SimpleLeftTreeItem[A](leftTree, item)
        case (Some(Right(leftTree)),Some(Left(rightItem))) => SimpleTreeItemItem[A](leftTree, item, rightItem)
        case (Some(Right(leftTree)),Some(Right(rightTree))) => SimpleTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
