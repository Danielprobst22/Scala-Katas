package sumtree

import scala.annotation.tailrec

object TailRecursiveImpl {

  def isValidSumTree(root: SumTreeNode): Boolean = {
    val initialStep = ExecutionStep.init(root)
    iterate(initialStep, root, List.empty)
  }

  @tailrec
  private def iterate(
    currentStep: ExecutionStep,
    lastVisitedNode: SumTreeNode,
    parentSteps: List[ExecutionStep],
  ): Boolean = {

    def nextChildNode: Option[SumTreeNode] = {

      def lastVisitedNodeIsLeftChild: Boolean = currentStep.node.leftChild.exists(_ eq lastVisitedNode)

      def lastVisitedNodeIsRightChild: Boolean = currentStep.node.rightChild.exists(_ eq lastVisitedNode)

      currentStep.node.leftChild
        .filterNot(_ => lastVisitedNodeIsLeftChild || lastVisitedNodeIsRightChild) // left child is always visited before right child -> if last visited node is right child, then left child was already visited 
        .orElse(currentStep.node.rightChild)
        .filterNot(_ => lastVisitedNodeIsRightChild)
    }

    def validateAndCalculateTotalSum: Either[IllegalArgumentException, BigInt] = {
      currentStep.accumulatedChildSum match {
        case Some(accumulatedChildSum) =>
          if (currentStep.node.value != accumulatedChildSum)
            Left(IllegalArgumentException(s"Node has value = ${currentStep.node.value}, but children have sum = $accumulatedChildSum"))
          else
            Right(currentStep.node.value + accumulatedChildSum)

        case None => Right(currentStep.node.value) // leaf nodes have no accumulated sum -> nothing to validate, just provide value to caller
      }
    }

    nextChildNode match {
      case Some(childNode) =>
        val childStep = ExecutionStep.init(childNode)
        iterate(childStep, currentStep.node, currentStep :: parentSteps)

      case None =>
        validateAndCalculateTotalSum match {
          case Right(totalSum) =>
            parentSteps match {
              case parentStep :: remainingParentSteps =>
                iterate(parentStep.addSum(totalSum), currentStep.node, remainingParentSteps)

              case Nil => true // root node has no parent step -> tree is valid sum tree
            }

          case Left(_) => false // error message is ignored -> might log it or do something else with it if it wasn't just a kata
        }
    }
  }

  private case class ExecutionStep(
    node: SumTreeNode,
    accumulatedChildSum: Option[BigInt],
  ) {

    def addSum(sum: BigInt): ExecutionStep = {
      val newAccumulated = accumulatedChildSum
        .map(_ + sum)
        .orElse(Some(sum))

      this.copy(accumulatedChildSum = newAccumulated)
    }
  }

  private object ExecutionStep {

    def init(node: SumTreeNode): ExecutionStep = ExecutionStep(node, None)
  }
}
