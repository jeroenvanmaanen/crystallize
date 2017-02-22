package org.leialearns.crystallize.immutabletree.blacknode

import org.leialearns.crystallize.immutabletree._

case class BlackNodeItem[+A](item: A) extends SingleNode[A,A,RedBlackNode[A]](item) with BlackNode[A] with Item[A,RedBlackNode[A]]
case class BlackNodeRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,RedBlackNode[A]](item, rightItem) with BlackNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class BlackNodeRightItemTree[+A](item: A, rightTree: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](item, rightTree) with BlackNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeItem[A,RedBlackNode[A]]
case class BlackNodeBucket[+A](bucket: RedBlackNode[A]) extends SingleNode[RedBlackNode[A],A,RedBlackNode[A]](bucket) with BlackNode[A] with Bucket[A,RedBlackNode[A]]
case class BlackNodeRightBucketItem[+A](bucket: RedBlackNode[A], rightItem: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](bucket, rightItem) with BlackNode[A]
  with RightItem[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class BlackNodeRightBucketTree[+A](bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](bucket, rightTree) with BlackNode[A]
  with RightTree[A,RedBlackNode[A]]
  with RightNodeBucket[A,RedBlackNode[A]]
case class BlackNodeLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,RedBlackNode[A]](leftItem, item) with BlackNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class BlackNodeItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,RedBlackNode[A]](leftItem, item, rightItem) with BlackNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BlackNodeItemItemTree[+A](leftItem: A, item: A, rightTree: RedBlackNode[A])
  extends BothNodes[A,A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, item, rightTree) with BlackNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BlackNodeLeftItemBucket[+A](leftItem: A, bucket: RedBlackNode[A])
  extends PairNode[A,RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket) with BlackNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftItem[A,RedBlackNode[A]]
case class BlackNodeItemBucketItem[+A](leftItem: A, bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[A,RedBlackNode[A],A,A,RedBlackNode[A]](leftItem, bucket, rightItem) with BlackNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BlackNodeItemBucketTree[+A](leftItem: A, bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[A,RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftItem, bucket, rightTree) with BlackNode[A]
  with LeftItem[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BlackNodeLeftTreeItem[+A](leftTree: RedBlackNode[A], item: A)
  extends PairNode[RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, item) with BlackNode[A]
  with LeftNodeItem[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class BlackNodeTreeItemItem[+A](leftTree: RedBlackNode[A], item: A, rightItem: A)
  extends BothNodes[RedBlackNode[A],A,A,A,RedBlackNode[A]](leftTree, item, rightItem) with BlackNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BlackNodeTreeItemTree[+A](leftTree: RedBlackNode[A], item: A, rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],A,RedBlackNode[A],A,RedBlackNode[A]](leftTree, item, rightTree) with BlackNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Item[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]
case class BlackNodeLeftTreeBucket[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A])
  extends PairNode[RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket) with BlackNode[A]
  with LeftNodeBucket[A,RedBlackNode[A]]
  with LeftTree[A,RedBlackNode[A]]
case class BlackNodeTreeBucketItem[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightItem: A)
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],A,A,RedBlackNode[A]](leftTree, bucket, rightItem) with BlackNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightItem[A,RedBlackNode[A]]
case class BlackNodeTreeBucketTree[+A](leftTree: RedBlackNode[A], bucket: RedBlackNode[A], rightTree: RedBlackNode[A])
  extends BothNodes[RedBlackNode[A],RedBlackNode[A],RedBlackNode[A],A,RedBlackNode[A]](leftTree, bucket, rightTree) with BlackNode[A]
  with LeftTree[A,RedBlackNode[A]]
  with Bucket[A,RedBlackNode[A]]
  with RightTree[A,RedBlackNode[A]]

// Factory object
object BlackNodeCases {
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
            case (None,None) => BlackNodeBucket[A](bucket)
            case (None,Some(Left(rightItem))) => BlackNodeRightBucketItem[A](bucket, rightItem)
            case (None,Some(Right(rightTree))) => BlackNodeRightBucketTree[A](bucket, rightTree)
            case (Some(Left(leftItem)),None) => BlackNodeLeftItemBucket[A](leftItem, bucket)
            case (Some(Left(leftItem)),Some(Left(rightItem))) => BlackNodeItemBucketItem[A](leftItem, bucket, rightItem)
            case (Some(Left(leftItem)),Some(Right(rightTree))) => BlackNodeItemBucketTree[A](leftItem, bucket, rightTree)
            case (Some(Right(leftTree)),None) => BlackNodeLeftTreeBucket[A](leftTree, bucket)
            case (Some(Right(leftTree)),Some(Left(rightItem))) => BlackNodeTreeBucketItem[A](leftTree, bucket, rightItem)
            case (Some(Right(leftTree)),Some(Right(rightTree))) => BlackNodeTreeBucketTree[A](leftTree, bucket, rightTree)
          }
      }
    }
    def createNode(leftNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], item: A, rightNodeOption: Option[TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A]], variant: Unit): TreeNodeTrait[A,RedBlackNode[A]] with RedBlackNode[A] = {
      (leftNodeOption map treeToEither, rightNodeOption map treeToEither) match {
        case (None,None) => BlackNodeItem[A](item)
        case (None,Some(Left(rightItem))) => BlackNodeRightItemItem[A](item, rightItem)
        case (None,Some(Right(rightTree))) => BlackNodeRightItemTree[A](item, rightTree)
        case (Some(Left(leftItem)),None) => BlackNodeLeftItemItem[A](leftItem, item)
        case (Some(Left(leftItem)),Some(Left(rightItem))) => BlackNodeItemItemItem[A](leftItem, item, rightItem)
        case (Some(Left(leftItem)),Some(Right(rightTree))) => BlackNodeItemItemTree[A](leftItem, item, rightTree)
        case (Some(Right(leftTree)),None) => BlackNodeLeftTreeItem[A](leftTree, item)
        case (Some(Right(leftTree)),Some(Left(rightItem))) => BlackNodeTreeItemItem[A](leftTree, item, rightItem)
        case (Some(Right(leftTree)),Some(Right(rightTree))) => BlackNodeTreeItemTree[A](leftTree, item, rightTree)
      }
    }
  }
}
