package sumtree

case class SumTreeNode(
  value: BigInt,
  leftChild: Option[SumTreeNode],
  rightChild: Option[SumTreeNode],
)

object SumTreeNode {

  def full(value: BigInt, leftChild: SumTreeNode, rightChild: SumTreeNode): SumTreeNode = {
    SumTreeNode(value, Some(leftChild), Some(rightChild))
  }

  def leftFull(value: BigInt, leftChild: SumTreeNode): SumTreeNode = {
    SumTreeNode(value, Some(leftChild), None)
  }

  def rightFull(value: BigInt, rightChild: SumTreeNode): SumTreeNode = {
    SumTreeNode(value, None, Some(rightChild))
  }

  def leaf(value: BigInt): SumTreeNode = {
    SumTreeNode(value, None, None)
  }
}
