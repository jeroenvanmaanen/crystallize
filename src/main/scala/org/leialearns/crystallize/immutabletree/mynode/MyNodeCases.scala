package org.leialearns.crystallize.immutabletree.mynode

import org.leialearns.crystallize.immutabletree._

case class MyNodeItem[A](item: A) extends SingleNode[A,A,MyNode[A]](item) with MyNode[A] with Item[A,MyNode[A]]
case class MyNodeRightItemItem[A](item: A, rightItem: A)
  extends PairNode[A,A,A,MyNode[A]](item, rightItem) with MyNode[A]
  with CoerceLeftItem[A,A,MyNode[A]] with RightItem[A,A,MyNode[A]] with CoerceRightItem[A,A,MyNode[A]]
case class MyNodeRightItemTree[A](item: A, rightTree: MyNode[A])
  extends PairNode[A,MyNode[A],A,MyNode[A]](item, rightTree) with MyNode[A]
  with CoerceLeftItem[A,MyNode[A],MyNode[A]] with RightItem[A,MyNode[A],MyNode[A]] with CoerceRightTree[A,A,MyNode[A]]
case class MyNodeBucket[A](bucket: MyNode[A]) extends SingleNode[MyNode[A],A,MyNode[A]](bucket) with MyNode[A] with Bucket[A,MyNode[A]]
case class MyNodeRightBucketItem[A](bucket: MyNode[A], rightItem: A)
  extends PairNode[MyNode[A],A,A,MyNode[A]](bucket, rightItem) with MyNode[A]
  with CoerceLeftTree[A,A,MyNode[A]] with RightTree[A,A,MyNode[A]] with CoerceRightItem[MyNode[A],A,MyNode[A]]
case class MyNodeRightBucketTree[A](bucket: MyNode[A], rightTree: MyNode[A])
  extends PairNode[MyNode[A],MyNode[A],A,MyNode[A]](bucket, rightTree) with MyNode[A]
  with CoerceLeftTree[A,MyNode[A],MyNode[A]] with RightTree[A,MyNode[A],MyNode[A]] with CoerceRightTree[MyNode[A],A,MyNode[A]]
case class MyNodeLeftItemItem[A](leftItem: A, item: A)
  extends PairNode[A,A,A,MyNode[A]](leftItem, item) with MyNode[A]
  with CoerceLeftItem[A,A,MyNode[A]] with LeftItem[A,A,MyNode[A]] with CoerceRightItem[A,A,MyNode[A]]
case class MyNodeItemItemItem[A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,MyNode[A]](leftItem, item, rightItem) with MyNode[A]
  with CoerceLeftItem[A,A,MyNode[A]] with CoerceRightItem[A,A,MyNode[A]]
  with CoerceMiddleItem[A,A,A,MyNode[A]]
case class MyNodeItemItemTree[A](leftItem: A, item: A, rightTree: MyNode[A])
  extends BothNodes[A,A,MyNode[A],A,MyNode[A]](leftItem, item, rightTree) with MyNode[A]
  with CoerceLeftItem[A,MyNode[A],MyNode[A]] with CoerceRightTree[A,A,MyNode[A]]
  with CoerceMiddleItem[A,A,MyNode[A],MyNode[A]]
case class MyNodeLeftItemBucket[A](leftItem: A, bucket: MyNode[A])
  extends PairNode[A,MyNode[A],A,MyNode[A]](leftItem, bucket) with MyNode[A]
  with CoerceLeftItem[A,MyNode[A],MyNode[A]] with LeftTree[A,A,MyNode[A]] with CoerceRightTree[A,A,MyNode[A]]
case class MyNodeItemBucketItem[A](leftItem: A, bucket: MyNode[A], rightItem: A)
  extends BothNodes[A,MyNode[A],A,A,MyNode[A]](leftItem, bucket, rightItem) with MyNode[A]
  with CoerceLeftItem[A,A,MyNode[A]] with CoerceRightItem[A,A,MyNode[A]]
  with CoerceMiddleBucket[A,A,A,MyNode[A]]
case class MyNodeItemBucketTree[A](leftItem: A, bucket: MyNode[A], rightTree: MyNode[A])
  extends BothNodes[A,MyNode[A],MyNode[A],A,MyNode[A]](leftItem, bucket, rightTree) with MyNode[A]
  with CoerceLeftItem[A,MyNode[A],MyNode[A]] with CoerceRightTree[A,A,MyNode[A]]
  with CoerceMiddleBucket[A,A,MyNode[A],MyNode[A]]
case class MyNodeLeftTreeItem[A](leftTree: MyNode[A], item: A)
  extends PairNode[MyNode[A],A,A,MyNode[A]](leftTree, item) with MyNode[A]
  with CoerceLeftTree[A,A,MyNode[A]] with LeftItem[MyNode[A],A,MyNode[A]] with CoerceRightItem[MyNode[A],A,MyNode[A]]
case class MyNodeTreeItemItem[A](leftTree: MyNode[A], item: A, rightItem: A)
  extends BothNodes[MyNode[A],A,A,A,MyNode[A]](leftTree, item, rightItem) with MyNode[A]
  with CoerceLeftTree[A,A,MyNode[A]] with CoerceRightItem[MyNode[A],A,MyNode[A]]
  with CoerceMiddleItem[MyNode[A],A,A,MyNode[A]]
case class MyNodeTreeItemTree[A](leftTree: MyNode[A], item: A, rightTree: MyNode[A])
  extends BothNodes[MyNode[A],A,MyNode[A],A,MyNode[A]](leftTree, item, rightTree) with MyNode[A]
  with CoerceLeftTree[A,MyNode[A],MyNode[A]] with CoerceRightTree[MyNode[A],A,MyNode[A]]
  with CoerceMiddleItem[MyNode[A],A,MyNode[A],MyNode[A]]
case class MyNodeLeftTreeBucket[A](leftTree: MyNode[A], bucket: MyNode[A])
  extends PairNode[MyNode[A],MyNode[A],A,MyNode[A]](leftTree, bucket) with MyNode[A]
  with CoerceLeftTree[A,MyNode[A],MyNode[A]] with LeftTree[MyNode[A],A,MyNode[A]] with CoerceRightTree[MyNode[A],A,MyNode[A]]
case class MyNodeTreeBucketItem[A](leftTree: MyNode[A], bucket: MyNode[A], rightItem: A)
  extends BothNodes[MyNode[A],MyNode[A],A,A,MyNode[A]](leftTree, bucket, rightItem) with MyNode[A]
  with CoerceLeftTree[A,A,MyNode[A]] with CoerceRightItem[MyNode[A],A,MyNode[A]]
  with CoerceMiddleBucket[MyNode[A],A,A,MyNode[A]]
case class MyNodeTreeBucketTree[A](leftTree: MyNode[A], bucket: MyNode[A], rightTree: MyNode[A])
  extends BothNodes[MyNode[A],MyNode[A],MyNode[A],A,MyNode[A]](leftTree, bucket, rightTree) with MyNode[A]
  with CoerceLeftTree[A,MyNode[A],MyNode[A]] with CoerceRightTree[MyNode[A],A,MyNode[A]]
  with CoerceMiddleBucket[MyNode[A],A,MyNode[A],MyNode[A]]

// Factory object
object MyNodeCases {
  def nodeFactory[A]: NodeFactory[A, MyNode[A]] = new NodeFactory[A, MyNode[A]] {
    def createNode(leftNodeOption: Option[MyNode[A]], middle: Either[A,MyNode[A]], rightNodeOption: Option[MyNode[A]]): MyNode[A] = {
      middle match {
        case Left(item) => createNode(leftNodeOption, item, rightNodeOption)
        case Right(bucket) => createNode(leftNodeOption, bucket, rightNodeOption)
      }
    }
    def createNode(leftNodeOption: Option[MyNode[A]], bucket: MyNode[A], rightNodeOption: Option[MyNode[A]]): MyNode[A] = {
      (leftNodeOption, bucket, rightNodeOption) match {
        case (None, MyNodeItem(item), None) => bucket
        case (_, MyNodeItem(item), _) => createNode(leftNodeOption, item, rightNodeOption)
        case _ =>
          (leftNodeOption, rightNodeOption) match {
            case (None,None) => MyNodeBucket[A](bucket)
            case (None,Some(MyNodeItem(rightItem))) => MyNodeRightBucketItem[A](bucket, rightItem)
            case (None,Some(rightTree)) => MyNodeRightBucketTree[A](bucket, rightTree)
            case (Some(MyNodeItem(leftItem)),None) => MyNodeLeftItemBucket[A](leftItem, bucket)
            case (Some(MyNodeItem(leftItem)),Some(MyNodeItem(rightItem))) => MyNodeItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(MyNodeItem(leftItem)),Some(rightTree)) => MyNodeItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(leftTree),None) => MyNodeLeftTreeBucket[A](leftTree, bucket)
            case (Some(leftTree),Some(MyNodeItem(rightItem))) => MyNodeTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(leftTree),Some(rightTree)) => MyNodeTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[MyNode[A]], item: A, rightNodeOption: Option[MyNode[A]]): MyNode[A] = {
      (leftNodeOption, rightNodeOption) match {
        case (None,None) => MyNodeItem[A](item)
        case (None,Some(MyNodeItem(rightItem))) => MyNodeRightItemItem[A](item, rightItem)
        case (None,Some(rightTree)) => MyNodeRightItemTree[A](item, rightTree)
        case (Some(MyNodeItem(leftItem)),None) => MyNodeLeftItemItem[A](leftItem, item)
        case (Some(MyNodeItem(leftItem)),Some(MyNodeItem(rightItem))) => MyNodeItemItemItem[A](leftItem, item, rightItem)
        case (Some(MyNodeItem(leftItem)),Some(rightTree)) => MyNodeItemItemTree[A](leftItem, item, rightTree)
        case (Some(leftTree),None) => MyNodeLeftTreeItem[A](leftTree, item)
        case (Some(leftTree),Some(MyNodeItem(rightItem))) => MyNodeTreeItemItem[A](leftTree, item, rightItem)
        case (Some(leftTree),Some(rightTree)) => MyNodeTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
