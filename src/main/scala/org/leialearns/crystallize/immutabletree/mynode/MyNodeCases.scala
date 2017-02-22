package org.leialearns.crystallize.immutabletree.mynode

import org.leialearns.crystallize.immutabletree._

case class MyNodeItem[+A](item: A) extends SingleNode[A,A,MyNode[A]](item) with MyNode[A] with Item[A,MyNode[A]]
case class MyNodeRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,MyNode[A]](item, rightItem) with MyNode[A]
  with RightItem[A,MyNode[A]]
  with RightNodeItem[A,MyNode[A]]
case class MyNodeRightItemTree[+A](item: A, rightTree: MyNode[A])
  extends PairNode[A,MyNode[A],A,MyNode[A]](item, rightTree) with MyNode[A]
  with RightTree[A,MyNode[A]]
  with RightNodeItem[A,MyNode[A]]
case class MyNodeBucket[+A](bucket: MyNode[A]) extends SingleNode[MyNode[A],A,MyNode[A]](bucket) with MyNode[A] with Bucket[A,MyNode[A]]
case class MyNodeRightBucketItem[+A](bucket: MyNode[A], rightItem: A)
  extends PairNode[MyNode[A],A,A,MyNode[A]](bucket, rightItem) with MyNode[A]
  with RightItem[A,MyNode[A]]
  with RightNodeBucket[A,MyNode[A]]
case class MyNodeRightBucketTree[+A](bucket: MyNode[A], rightTree: MyNode[A])
  extends PairNode[MyNode[A],MyNode[A],A,MyNode[A]](bucket, rightTree) with MyNode[A]
  with RightTree[A,MyNode[A]]
  with RightNodeBucket[A,MyNode[A]]
case class MyNodeLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,MyNode[A]](leftItem, item) with MyNode[A]
  with LeftNodeItem[A,MyNode[A]]
  with LeftItem[A,MyNode[A]]
case class MyNodeItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,MyNode[A]](leftItem, item, rightItem) with MyNode[A]
  with LeftItem[A,MyNode[A]]
  with Item[A,MyNode[A]]
  with RightItem[A,MyNode[A]]
case class MyNodeItemItemTree[+A](leftItem: A, item: A, rightTree: MyNode[A])
  extends BothNodes[A,A,MyNode[A],A,MyNode[A]](leftItem, item, rightTree) with MyNode[A]
  with LeftItem[A,MyNode[A]]
  with Item[A,MyNode[A]]
  with RightTree[A,MyNode[A]]
case class MyNodeLeftItemBucket[+A](leftItem: A, bucket: MyNode[A])
  extends PairNode[A,MyNode[A],A,MyNode[A]](leftItem, bucket) with MyNode[A]
  with LeftNodeBucket[A,MyNode[A]]
  with LeftItem[A,MyNode[A]]
case class MyNodeItemBucketItem[+A](leftItem: A, bucket: MyNode[A], rightItem: A)
  extends BothNodes[A,MyNode[A],A,A,MyNode[A]](leftItem, bucket, rightItem) with MyNode[A]
  with LeftItem[A,MyNode[A]]
  with Bucket[A,MyNode[A]]
  with RightItem[A,MyNode[A]]
case class MyNodeItemBucketTree[+A](leftItem: A, bucket: MyNode[A], rightTree: MyNode[A])
  extends BothNodes[A,MyNode[A],MyNode[A],A,MyNode[A]](leftItem, bucket, rightTree) with MyNode[A]
  with LeftItem[A,MyNode[A]]
  with Bucket[A,MyNode[A]]
  with RightTree[A,MyNode[A]]
case class MyNodeLeftTreeItem[+A](leftTree: MyNode[A], item: A)
  extends PairNode[MyNode[A],A,A,MyNode[A]](leftTree, item) with MyNode[A]
  with LeftNodeItem[A,MyNode[A]]
  with LeftTree[A,MyNode[A]]
case class MyNodeTreeItemItem[+A](leftTree: MyNode[A], item: A, rightItem: A)
  extends BothNodes[MyNode[A],A,A,A,MyNode[A]](leftTree, item, rightItem) with MyNode[A]
  with LeftTree[A,MyNode[A]]
  with Item[A,MyNode[A]]
  with RightItem[A,MyNode[A]]
case class MyNodeTreeItemTree[+A](leftTree: MyNode[A], item: A, rightTree: MyNode[A])
  extends BothNodes[MyNode[A],A,MyNode[A],A,MyNode[A]](leftTree, item, rightTree) with MyNode[A]
  with LeftTree[A,MyNode[A]]
  with Item[A,MyNode[A]]
  with RightTree[A,MyNode[A]]
case class MyNodeLeftTreeBucket[+A](leftTree: MyNode[A], bucket: MyNode[A])
  extends PairNode[MyNode[A],MyNode[A],A,MyNode[A]](leftTree, bucket) with MyNode[A]
  with LeftNodeBucket[A,MyNode[A]]
  with LeftTree[A,MyNode[A]]
case class MyNodeTreeBucketItem[+A](leftTree: MyNode[A], bucket: MyNode[A], rightItem: A)
  extends BothNodes[MyNode[A],MyNode[A],A,A,MyNode[A]](leftTree, bucket, rightItem) with MyNode[A]
  with LeftTree[A,MyNode[A]]
  with Bucket[A,MyNode[A]]
  with RightItem[A,MyNode[A]]
case class MyNodeTreeBucketTree[+A](leftTree: MyNode[A], bucket: MyNode[A], rightTree: MyNode[A])
  extends BothNodes[MyNode[A],MyNode[A],MyNode[A],A,MyNode[A]](leftTree, bucket, rightTree) with MyNode[A]
  with LeftTree[A,MyNode[A]]
  with Bucket[A,MyNode[A]]
  with RightTree[A,MyNode[A]]

// Factory object
object MyNodeCases {
  def treeToEither[A,T](tree: TreeNodeTrait[A,T] with T): Either[A,TreeNodeTrait[A,T] with T] = {
    if (tree.getLeftNode.isEmpty && tree.getRightNode.isEmpty) tree.getMiddle else Right(tree)
  }
  def nodeFactory[A]: NodeFactory[A, MyNode[A], Unit] = new NodeFactory[A, MyNode[A], Unit] {
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,MyNode[A]] with MyNode[A]], bucket: TreeNodeTrait[A,MyNode[A]] with MyNode[A], rightNodeOption: Option[TreeNodeTrait[A,MyNode[A]] with MyNode[A]], variant: Unit): TreeNodeTrait[A,MyNode[A]] with MyNode[A] = {
      val middle: Either[A,_] = treeToEither(bucket)
      (leftNodeOption, middle, rightNodeOption) match {
        case (None, _, None) => bucket
        case (_, Left(item), _) => createNode(leftNodeOption, item, rightNodeOption, variant)
        case _ =>
          (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
            case (None,None) => MyNodeBucket[A](bucket)
            case (None,Some(Left(rightItem))) => MyNodeRightBucketItem[A](bucket, rightItem)
            case (None,Some(Right(rightTree))) => MyNodeRightBucketTree[A](bucket, rightTree)
            case (Some(Left(leftItem)),None) => MyNodeLeftItemBucket[A](leftItem, bucket)
            case (Some(Left(leftItem)),Some(Left(rightItem))) => MyNodeItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(Left(leftItem)),Some(Right(rightTree))) => MyNodeItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(Right(leftTree)),None) => MyNodeLeftTreeBucket[A](leftTree, bucket)
            case (Some(Right(leftTree)),Some(Left(rightItem))) => MyNodeTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(Right(leftTree)),Some(Right(rightTree))) => MyNodeTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,MyNode[A]] with MyNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,MyNode[A]] with MyNode[A]], variant: Unit): TreeNodeTrait[A,MyNode[A]] with MyNode[A] = {
      (leftNodeOption map (treeToEither(_)), rightNodeOption map (treeToEither(_))) match {
        case (None,None) => MyNodeItem[A](item)
        case (None,Some(Left(rightItem))) => MyNodeRightItemItem[A](item, rightItem)
        case (None,Some(Right(rightTree))) => MyNodeRightItemTree[A](item, rightTree)
        case (Some(Left(leftItem)),None) => MyNodeLeftItemItem[A](leftItem, item)
        case (Some(Left(leftItem)),Some(Left(rightItem))) => MyNodeItemItemItem[A](leftItem, item, rightItem)
        case (Some(Left(leftItem)),Some(Right(rightTree))) => MyNodeItemItemTree[A](leftItem, item, rightTree)
        case (Some(Right(leftTree)),None) => MyNodeLeftTreeItem[A](leftTree, item)
        case (Some(Right(leftTree)),Some(Left(rightItem))) => MyNodeTreeItemItem[A](leftTree, item, rightItem)
        case (Some(Right(leftTree)),Some(Right(rightTree))) => MyNodeTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
