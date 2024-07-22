package sumtree

import org.scalatest.prop.TableFor1
import org.scalatest.prop.Tables.Table

import scala.annotation.tailrec

object SumTreeTestUtil {

  lazy val EMPTY_TREE: SumTreeNode = SumTreeNode.leaf(BigInt(42))

  lazy val VALID_TREES: TableFor1[SumTreeNode] = Table("valid tree", VALID_TREE_1, VALID_TREE_2, VALID_TREE_3, VALID_TREE_4)
  /*
        26
       /  \
     10    3
    /  \    \
   4    6    3
   */
  private lazy val VALID_TREE_1: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
    SumTreeNode.rightFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
  )
  /*
         26
       /    \
     10      3
    /  \    /
   4    6  3
   */
  private lazy val VALID_TREE_2: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
    SumTreeNode.leftFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
  )
  /*
         26
       /    \
      3     10
      \    /  \
       3  4    6
   */
  private lazy val VALID_TREE_3: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.rightFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
  )
  /*
         26
       /    \
      3     10
     /     /  \
    3     4    6
   */
  private lazy val VALID_TREE_4: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.leftFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
  )

  lazy val INVALID_TREES: TableFor1[SumTreeNode] = Table("invalid tree", TREE_WITH_INVALID_LEFT_CHILD, TREE_WITH_INVALID_RIGHT_CHILD, TREE_WITH_INVALID_ROOT)
  /*
        26
       /  \
     11    3
    /  \    \
   4    6    3
   */
  private lazy val TREE_WITH_INVALID_LEFT_CHILD: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.full(
      BigInt(11),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
    SumTreeNode.rightFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
  )
  /*
        26
       /  \
     10    4
    /  \    \
   4    6    3
   */
  private lazy val TREE_WITH_INVALID_RIGHT_CHILD: SumTreeNode = SumTreeNode.full(
    BigInt(26),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
    SumTreeNode.rightFull(
      BigInt(4),
      SumTreeNode.leaf(BigInt(3)),
    ),
  )
  /*
        27
       /  \
     10    3
    /  \    \
   4    6    3
   */
  private lazy val TREE_WITH_INVALID_ROOT: SumTreeNode = SumTreeNode.full(
    BigInt(27),
    SumTreeNode.full(
      BigInt(10),
      SumTreeNode.leaf(BigInt(4)),
      SumTreeNode.leaf(BigInt(6)),
    ),
    SumTreeNode.rightFull(
      BigInt(3),
      SumTreeNode.leaf(BigInt(3)),
    ),
  )

  lazy val UNBALANCED_TREES_WITH_DEPTH_TEN_THOUSAND: TableFor1[SumTreeNode] = Table(
    "unbalanced tree",
    LEFT_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND,
    RIGHT_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND,
    RANDOM_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND,
  )
  private lazy val LEFT_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND: SumTreeNode = makeUnbalancedSumTree(10_000, BranchingDirection.Left)
  private lazy val RIGHT_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND: SumTreeNode = makeUnbalancedSumTree(10_000, BranchingDirection.Right)
  private lazy val RANDOM_UNBALANCED_TREE_WITH_DEPTH_TEN_THOUSAND: SumTreeNode = makeUnbalancedSumTree(10_000, BranchingDirection.Random)

  private def makeUnbalancedSumTree(depth: Int, branchingDirection: BranchingDirection): SumTreeNode = {
    val valueIterator = sumTreeNodeValueIterator

    @tailrec
    def iterate(childNode: SumTreeNode, currentDepth: Int): SumTreeNode = {
      val currentNode = branchingDirection.makeNode(valueIterator.next(), childNode)

      if (currentDepth == 0)
        currentNode
      else
        iterate(currentNode, currentDepth - 1)
    }

    iterate(SumTreeNode.leaf(valueIterator.next()), depth)
  }

  private def sumTreeNodeValueIterator: Iterator[BigInt] = {
    LazyList
      .iterate(BigInt(1))(v => v * 2)
      .prepended(BigInt(1)) // values should be: 1, 1, 2, 4, 8, 16, etc
      .iterator
  }

  private enum BranchingDirection {
    case Left, Right, Random

    def makeNode(value: BigInt, childNode: SumTreeNode): SumTreeNode = this match {
      case BranchingDirection.Left => SumTreeNode.leftFull(value, childNode)
      case BranchingDirection.Right => SumTreeNode.rightFull(value, childNode)
      case BranchingDirection.Random =>
        if (scala.util.Random.nextBoolean())
          SumTreeNode.leftFull(value, childNode)
        else
          SumTreeNode.rightFull(value, childNode)
    }
  }
}
