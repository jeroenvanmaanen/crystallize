package org.leialearns.crystallize.immutabletree.mynode

import org.leialearns.crystallize.immutabletree._

case class MyNodeItem[+A](item: A) extends SingleNode[A,A,TreeNodeTrait](item) with MyNode[A] with Item[A,TreeNodeTrait]
case class MyNodeRightItemItem[+A](item: A, rightItem: A)
  extends PairNode[A,A,A,TreeNodeTrait](item, rightItem) with MyNode[A]
  with RightItem[A,TreeNodeTrait]
  with RightNodeItem[A,TreeNodeTrait]
case class MyNodeRightItemTree[+A](item: A, rightTree: TreeNodeTrait)
  extends PairNode[A,TreeNodeTrait,A,TreeNodeTrait](item, rightTree) with MyNode[A]
  with RightItem[A,TreeNodeTrait]
  with RightNodeItem[A,TreeNodeTrait]
case class MyNodeBucket[+A](bucket: TreeNodeTrait) extends SingleNode[TreeNodeTrait,A,TreeNodeTrait](bucket) with MyNode[A] with Bucket[A,TreeNodeTrait]
case class MyNodeRightBucketItem[+A](bucket: TreeNodeTrait, rightItem: A)
  extends PairNode[TreeNodeTrait,A,A,TreeNodeTrait](bucket, rightItem) with MyNode[A]
  with RightTree[A,TreeNodeTrait]
  with RightNodeBucket[A,TreeNodeTrait]
case class MyNodeRightBucketTree[+A](bucket: TreeNodeTrait, rightTree: TreeNodeTrait)
  extends PairNode[TreeNodeTrait,TreeNodeTrait,A,TreeNodeTrait](bucket, rightTree) with MyNode[A]
  with RightTree[A,TreeNodeTrait]
  with RightNodeBucket[A,TreeNodeTrait]
case class MyNodeLeftItemItem[+A](leftItem: A, item: A)
  extends PairNode[A,A,A,TreeNodeTrait](leftItem, item) with MyNode[A]
  with LeftNodeItem[A,TreeNodeTrait]
  with LeftItem[A,TreeNodeTrait]
case class MyNodeItemItemItem[+A](leftItem: A, item: A, rightItem: A)
  extends BothNodes[A,A,A,A,TreeNodeTrait](leftItem, item, rightItem) with MyNode[A]
  with LeftItem[A,TreeNodeTrait]
  with Item[A,TreeNodeTrait]
  with RightItem[A,TreeNodeTrait]
case class MyNodeItemItemTree[+A](leftItem: A, item: A, rightTree: TreeNodeTrait)
  extends BothNodes[A,A,TreeNodeTrait,A,TreeNodeTrait](leftItem, item, rightTree) with MyNode[A]
  with LeftItem[A,TreeNodeTrait]
  with Item[A,TreeNodeTrait]
  with RightTree[A,TreeNodeTrait]
case class MyNodeLeftItemBucket[+A](leftItem: A, bucket: TreeNodeTrait)
  extends PairNode[A,TreeNodeTrait,A,TreeNodeTrait](leftItem, bucket) with MyNode[A]
  with LeftNodeBucket[A,TreeNodeTrait]
  with LeftTree[A,TreeNodeTrait]
case class MyNodeItemBucketItem[+A](leftItem: A, bucket: TreeNodeTrait, rightItem: A)
  extends BothNodes[A,TreeNodeTrait,A,A,TreeNodeTrait](leftItem, bucket, rightItem) with MyNode[A]
  with LeftItem[A,TreeNodeTrait]
  with Bucket[A,TreeNodeTrait]
  with RightItem[A,TreeNodeTrait]
case class MyNodeItemBucketTree[+A](leftItem: A, bucket: TreeNodeTrait, rightTree: TreeNodeTrait)
  extends BothNodes[A,TreeNodeTrait,TreeNodeTrait,A,TreeNodeTrait](leftItem, bucket, rightTree) with MyNode[A]
  with LeftItem[A,TreeNodeTrait]
  with Bucket[A,TreeNodeTrait]
  with RightTree[A,TreeNodeTrait]
case class MyNodeLeftTreeItem[+A](leftTree: TreeNodeTrait, item: A)
  extends PairNode[TreeNodeTrait,A,A,TreeNodeTrait](leftTree, item) with MyNode[A]
  with LeftNodeItem[A,TreeNodeTrait]
  with LeftItem[A,TreeNodeTrait]
case class MyNodeTreeItemItem[+A](leftTree: TreeNodeTrait, item: A, rightItem: A)
  extends BothNodes[TreeNodeTrait,A,A,A,TreeNodeTrait](leftTree, item, rightItem) with MyNode[A]
  with LeftTree[A,TreeNodeTrait]
  with Item[A,TreeNodeTrait]
  with RightItem[A,TreeNodeTrait]
case class MyNodeTreeItemTree[+A](leftTree: TreeNodeTrait, item: A, rightTree: TreeNodeTrait)
  extends BothNodes[TreeNodeTrait,A,TreeNodeTrait,A,TreeNodeTrait](leftTree, item, rightTree) with MyNode[A]
  with LeftTree[A,TreeNodeTrait]
  with Item[A,TreeNodeTrait]
  with RightTree[A,TreeNodeTrait]
case class MyNodeLeftTreeBucket[+A](leftTree: TreeNodeTrait, bucket: TreeNodeTrait)
  extends PairNode[TreeNodeTrait,TreeNodeTrait,A,TreeNodeTrait](leftTree, bucket) with MyNode[A]
  with LeftNodeBucket[A,TreeNodeTrait]
  with LeftTree[A,TreeNodeTrait]
case class MyNodeTreeBucketItem[+A](leftTree: TreeNodeTrait, bucket: TreeNodeTrait, rightItem: A)
  extends BothNodes[TreeNodeTrait,TreeNodeTrait,A,A,TreeNodeTrait](leftTree, bucket, rightItem) with MyNode[A]
  with LeftTree[A,TreeNodeTrait]
  with Bucket[A,TreeNodeTrait]
  with RightItem[A,TreeNodeTrait]
case class MyNodeTreeBucketTree[+A](leftTree: TreeNodeTrait, bucket: TreeNodeTrait, rightTree: TreeNodeTrait)
  extends BothNodes[TreeNodeTrait,TreeNodeTrait,TreeNodeTrait,A,TreeNodeTrait](leftTree, bucket, rightTree) with MyNode[A]
  with LeftTree[A,TreeNodeTrait]
  with Bucket[A,TreeNodeTrait]
  with RightTree[A,TreeNodeTrait]

// Factory object
object MyNodeCases {
  def treeToEither[A,T <: TreeNodeTrait[A,T]](tree: T): Either[A,T] = {
    if (tree.getLeftNode.isEmpty && tree.getRightNode.isEmpty) tree.getMiddle else Right(tree)
  }
  def nodeFactory[A]: NodeFactory[A, TreeNodeTrait, Unit] = new NodeFactory[A, TreeNodeTrait, Unit] {
    def createNode(leftNodeOption: Option[TreeNodeTrait], bucket: TreeNodeTrait, rightNodeOption: Option[TreeNodeTrait], variant: Unit): TreeNodeTrait = {
      val middle: Either[A,TreeNodeTrait] = treeToEither(bucket)
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
    def createNode(leftNodeOption: Option[TreeNodeTrait], item: A, rightNodeOption: Option[TreeNodeTrait], variant: Unit): TreeNodeTrait = {
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
