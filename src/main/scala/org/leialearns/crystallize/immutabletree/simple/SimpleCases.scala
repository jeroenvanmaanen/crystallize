package org.leialearns.crystallize.immutabletree.simple

import org.leialearns.crystallize.immutabletree._

case class SimpleItem[A](item: A) extends SingleNode[A,A,Simple[A]](item) with Simple[A] with Item[A,Simple[A]]
case class SimpleRightItemItem[A](item: A, rightItem: A)
  extends PairNode[A,A,A,Simple[A]](item, rightItem) with Simple[A]
  with CoerceLeftItem[A,A,Simple[A]] with RightItem[A,A,Simple[A]] with CoerceRightItem[A,A,Simple[A]]
case class SimpleRightItemTree[A](item: A, rightTree: Simple[A])
  extends PairNode[A,Simple[A],A,Simple[A]](item, rightTree) with Simple[A]
  with CoerceLeftItem[A,Simple[A],Simple[A]] with RightItem[A,Simple[A],Simple[A]] with CoerceRightTree[A,A,Simple[A]]
case class SimpleBucket[A](bucket: Simple[A]) extends SingleNode[Simple[A],A,Simple[A]](bucket) with Simple[A] with Bucket[A,Simple[A]]
case class SimpleRightBucketItem[A](bucket: Simple[A], rightItem: A)
  extends PairNode[Simple[A],A,A,Simple[A]](bucket, rightItem) with Simple[A]
  with CoerceLeftTree[A,A,Simple[A]] with RightTree[A,A,Simple[A]] with CoerceRightItem[Simple[A],A,Simple[A]]
case class SimpleRightBucketTree[A](bucket: Simple[A], rightTree: Simple[A])
  extends PairNode[Simple[A],Simple[A],A,Simple[A]](bucket, rightTree) with Simple[A]
  with CoerceLeftTree[A,Simple[A],Simple[A]] with RightTree[A,Simple[A],Simple[A]] with CoerceRightTree[Simple[A],A,Simple[A]]
case class SimpleLeftItemItem[A](leftItem: A, item: A)
  extends PairNode[A,A,A,Simple[A]](leftItem, item) with Simple[A]
  with CoerceLeftItem[A,A,Simple[A]] with LeftItem[A,A,Simple[A]] with CoerceRightItem[A,A,Simple[A]]
case class SimpleItemItemItem[A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,Simple[A]](leftItem, item, rightItem) with Simple[A]
  with CoerceLeftItem[A,A,Simple[A]] with CoerceRightItem[A,A,Simple[A]]
  with CoerceMiddleItem[A,A,A,Simple[A]]
case class SimpleItemItemTree[A](leftItem: A, item: A, rightTree: Simple[A])
  extends BothNodes[A,A,Simple[A],A,Simple[A]](leftItem, item, rightTree) with Simple[A]
  with CoerceLeftItem[A,Simple[A],Simple[A]] with CoerceRightTree[A,A,Simple[A]]
  with CoerceMiddleItem[A,A,Simple[A],Simple[A]]
case class SimpleLeftItemBucket[A](leftItem: A, bucket: Simple[A])
  extends PairNode[A,Simple[A],A,Simple[A]](leftItem, bucket) with Simple[A]
  with CoerceLeftItem[A,Simple[A],Simple[A]] with LeftTree[A,A,Simple[A]] with CoerceRightTree[A,A,Simple[A]]
case class SimpleItemBucketItem[A](leftItem: A, bucket: Simple[A], rightItem: A)
  extends BothNodes[A,Simple[A],A,A,Simple[A]](leftItem, bucket, rightItem) with Simple[A]
  with CoerceLeftItem[A,A,Simple[A]] with CoerceRightItem[A,A,Simple[A]]
  with CoerceMiddleBucket[A,A,A,Simple[A]]
case class SimpleItemBucketTree[A](leftItem: A, bucket: Simple[A], rightTree: Simple[A])
  extends BothNodes[A,Simple[A],Simple[A],A,Simple[A]](leftItem, bucket, rightTree) with Simple[A]
  with CoerceLeftItem[A,Simple[A],Simple[A]] with CoerceRightTree[A,A,Simple[A]]
  with CoerceMiddleBucket[A,A,Simple[A],Simple[A]]
case class SimpleLeftTreeItem[A](leftTree: Simple[A], item: A)
  extends PairNode[Simple[A],A,A,Simple[A]](leftTree, item) with Simple[A]
  with CoerceLeftTree[A,A,Simple[A]] with LeftItem[Simple[A],A,Simple[A]] with CoerceRightItem[Simple[A],A,Simple[A]]
case class SimpleTreeItemItem[A](leftTree: Simple[A], item: A, rightItem: A)
  extends BothNodes[Simple[A],A,A,A,Simple[A]](leftTree, item, rightItem) with Simple[A]
  with CoerceLeftTree[A,A,Simple[A]] with CoerceRightItem[Simple[A],A,Simple[A]]
  with CoerceMiddleItem[Simple[A],A,A,Simple[A]]
case class SimpleTreeItemTree[A](leftTree: Simple[A], item: A, rightTree: Simple[A])
  extends BothNodes[Simple[A],A,Simple[A],A,Simple[A]](leftTree, item, rightTree) with Simple[A]
  with CoerceLeftTree[A,Simple[A],Simple[A]] with CoerceRightTree[Simple[A],A,Simple[A]]
  with CoerceMiddleItem[Simple[A],A,Simple[A],Simple[A]]
case class SimpleLeftTreeBucket[A](leftTree: Simple[A], bucket: Simple[A])
  extends PairNode[Simple[A],Simple[A],A,Simple[A]](leftTree, bucket) with Simple[A]
  with CoerceLeftTree[A,Simple[A],Simple[A]] with LeftTree[Simple[A],A,Simple[A]] with CoerceRightTree[Simple[A],A,Simple[A]]
case class SimpleTreeBucketItem[A](leftTree: Simple[A], bucket: Simple[A], rightItem: A)
  extends BothNodes[Simple[A],Simple[A],A,A,Simple[A]](leftTree, bucket, rightItem) with Simple[A]
  with CoerceLeftTree[A,A,Simple[A]] with CoerceRightItem[Simple[A],A,Simple[A]]
  with CoerceMiddleBucket[Simple[A],A,A,Simple[A]]
case class SimpleTreeBucketTree[A](leftTree: Simple[A], bucket: Simple[A], rightTree: Simple[A])
  extends BothNodes[Simple[A],Simple[A],Simple[A],A,Simple[A]](leftTree, bucket, rightTree) with Simple[A]
  with CoerceLeftTree[A,Simple[A],Simple[A]] with CoerceRightTree[Simple[A],A,Simple[A]]
  with CoerceMiddleBucket[Simple[A],A,Simple[A],Simple[A]]

// Factory object
object SimpleCases {
  def nodeFactory[A]: NodeFactory[A, Simple[A]] = new NodeFactory[A, Simple[A]] {
    def createNode(leftNodeOption: Option[Simple[A]], middle: Either[A,Simple[A]], rightNodeOption: Option[Simple[A]]): Simple[A] = {
      middle match {
        case Left(item) => createNode(leftNodeOption, item, rightNodeOption)
        case Right(bucket) => createNode(leftNodeOption, bucket, rightNodeOption)
      }
    }
    def createNode(leftNodeOption: Option[Simple[A]], bucket: Simple[A], rightNodeOption: Option[Simple[A]]): Simple[A] = {
      (leftNodeOption, bucket, rightNodeOption) match {
        case (None, SimpleItem(item), None) => bucket
        case (_, SimpleItem(item), _) => createNode(leftNodeOption, item, rightNodeOption)
        case _ =>
          (leftNodeOption, rightNodeOption) match {
            case (None,None) => SimpleBucket[A](bucket)
            case (None,Some(SimpleItem(rightItem))) => SimpleRightBucketItem[A](bucket, rightItem)
            case (None,Some(rightTree)) => SimpleRightBucketTree[A](bucket, rightTree)
            case (Some(SimpleItem(leftItem)),None) => SimpleLeftItemBucket[A](leftItem, bucket)
            case (Some(SimpleItem(leftItem)),Some(SimpleItem(rightItem))) => SimpleItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(SimpleItem(leftItem)),Some(rightTree)) => SimpleItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(leftTree),None) => SimpleLeftTreeBucket[A](leftTree, bucket)
            case (Some(leftTree),Some(SimpleItem(rightItem))) => SimpleTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(leftTree),Some(rightTree)) => SimpleTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[Simple[A]], item: A, rightNodeOption: Option[Simple[A]]): Simple[A] = {
      (leftNodeOption, rightNodeOption) match {
        case (None,None) => SimpleItem[A](item)
        case (None,Some(SimpleItem(rightItem))) => SimpleRightItemItem[A](item, rightItem)
        case (None,Some(rightTree)) => SimpleRightItemTree[A](item, rightTree)
        case (Some(SimpleItem(leftItem)),None) => SimpleLeftItemItem[A](leftItem, item)
        case (Some(SimpleItem(leftItem)),Some(SimpleItem(rightItem))) => SimpleItemItemItem[A](leftItem, item, rightItem)
        case (Some(SimpleItem(leftItem)),Some(rightTree)) => SimpleItemItemTree[A](leftItem, item, rightTree)
        case (Some(leftTree),None) => SimpleLeftTreeItem[A](leftTree, item)
        case (Some(leftTree),Some(SimpleItem(rightItem))) => SimpleTreeItemItem[A](leftTree, item, rightItem)
        case (Some(leftTree),Some(rightTree)) => SimpleTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
