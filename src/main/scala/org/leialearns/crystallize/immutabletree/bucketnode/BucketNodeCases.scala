package org.leialearns.crystallize.immutabletree.bucketnode

import org.leialearns.crystallize.immutabletree._

case class BucketNodeItem[+A](item: A) extends SingleNode[A,A,RedBlackNode[A]](item) with BucketNode[A] with Item[A,RedBlackNode[A]]
case class BucketNodeRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,RedBlackNode[A]](item, rightItem) with BucketNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class BucketNodeRightItemTree[+A](item: A, rightTree: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](item, rightTree) with BucketNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class BucketNodeBucket[+A](bucket: RedBlackNode[A]) extends SingleNode[RedBlackNode[A],A,RedBlackNode[A]](bucket) with BucketNode[A] with Bucket[A,RedBlackNode[A]]
case class BucketNodeRightBucketItem[+A](bucket: RedBlackNode[A], rightItem: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](bucket, rightItem) with BucketNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class BucketNodeRightBucketTree[+A](bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](bucket, rightTree) with BucketNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class BucketNodeLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,RedBlackNode[A]](leftItem, item) with BucketNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class BucketNodeItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,RedBlackNode[A]](leftItem, item, rightItem) with BucketNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BucketNodeItemItemTree[+A](leftItem: A, item: A, rightTree: RedBlackNode[A])
  extends BothNodes[A,A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, item, rightTree) with BucketNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BucketNodeLeftItemBucket[+A](leftItem: A, bucket: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket) with BucketNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class BucketNodeItemBucketItem[+A](leftItem: A, bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[A,RedBlackNode[A],A,A,RedBlackNode[A]](leftItem, bucket, rightItem) with BucketNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BucketNodeItemBucketTree[+A](leftItem: A, bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[A,RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket, rightTree) with BucketNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BucketNodeLeftTreeItem[+A](leftTree: RedBlackNode[A], item: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, item) with BucketNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class BucketNodeTreeItemItem[+A](leftTree: RedBlackNode[A], item: A, rightItem: A)
  extends BothNodes[RedBlackNode[A],A,A,A,RedBlackNode[A]](leftTree, item, rightItem) with BucketNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BucketNodeTreeItemTree[+A](leftTree: RedBlackNode[A], item: A, rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],A,RedBlackNode[A],A,RedBlackNode[A]](leftTree, item, rightTree) with BucketNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BucketNodeLeftTreeBucket[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket) with BucketNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class BucketNodeTreeBucketItem[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, bucket, rightItem) with BucketNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BucketNodeTreeBucketTree[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket, rightTree) with BucketNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]

// Factory object
object BucketNodeCases {
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
          (leftNodeOption map treeToEither, rightNodeOption map treeToEither) match {
            case (None,None) => BucketNodeBucket[A](bucket)
            case (None,Some(Left(rightItem))) => BucketNodeRightBucketItem[A](bucket, rightItem)
            case (None,Some(Right(rightTree))) => BucketNodeRightBucketTree[A](bucket, rightTree)
            case (Some(Left(leftItem)),None) => BucketNodeLeftItemBucket[A](leftItem, bucket)
            case (Some(Left(leftItem)),Some(Left(rightItem))) => BucketNodeItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(Left(leftItem)),Some(Right(rightTree))) => BucketNodeItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(Right(leftTree)),None) => BucketNodeLeftTreeBucket[A](leftTree, bucket)
            case (Some(Right(leftTree)),Some(Left(rightItem))) => BucketNodeTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(Right(leftTree)),Some(Right(rightTree))) => BucketNodeTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], variant: Unit): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      (leftNodeOption map treeToEither, rightNodeOption map treeToEither) match {
        case (None,None) => BucketNodeItem[A](item)
        case (None,Some(Left(rightItem))) => BucketNodeRightItemItem[A](item, rightItem)
        case (None,Some(Right(rightTree))) => BucketNodeRightItemTree[A](item, rightTree)
        case (Some(Left(leftItem)),None) => BucketNodeLeftItemItem[A](leftItem, item)
        case (Some(Left(leftItem)),Some(Left(rightItem))) => BucketNodeItemItemItem[A](leftItem, item, rightItem)
        case (Some(Left(leftItem)),Some(Right(rightTree))) => BucketNodeItemItemTree[A](leftItem, item, rightTree)
        case (Some(Right(leftTree)),None) => BucketNodeLeftTreeItem[A](leftTree, item)
        case (Some(Right(leftTree)),Some(Left(rightItem))) => BucketNodeTreeItemItem[A](leftTree, item, rightItem)
        case (Some(Right(leftTree)),Some(Right(rightTree))) => BucketNodeTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
