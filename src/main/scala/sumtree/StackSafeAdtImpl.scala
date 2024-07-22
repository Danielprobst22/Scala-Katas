package sumtree

import util.Util.*

import scala.annotation.tailrec

object StackSafeAdtImpl {

  def isValidSumTree(root: SumTreeNode): Boolean = {
    @tailrec
    def iterate(currentStep: CurrentStep): Boolean = currentStep.next match {
      case NextStep.Continue(next) => iterate(next)
      case NextStep.Yield(result) => result
    }

    iterate(CurrentStep.initial(root))
  }


  private sealed trait ParentStep {

    def provideSum(childSum: Option[BigInt]): NextStep
  }

  private object ParentStep {

    case class AwaitingLeftChild(
      private val parentStep: Option[ParentStep],
      private val currentNode: SumTreeNode,
    ) extends ParentStep {

      override def provideSum(leftChildSum: Option[BigInt]): NextStep = {
        val awaitingRightChildOfCurrent = AwaitingRightChild(parentStep, currentNode, leftChildSum)
        val visitRightChildOfCurrent = CurrentStep.VisitChild(awaitingRightChildOfCurrent, currentNode, CurrentStep.ChildToVisit.Right)
        NextStep.Continue(visitRightChildOfCurrent)
      }
    }

    private case class AwaitingRightChild(
      private val parentStep: Option[ParentStep],
      private val currentNode: SumTreeNode,
      private val leftChildSum: Option[BigInt],
    ) extends ParentStep {

      override def provideSum(rightChildSum: Option[BigInt]): NextStep = {
        val validateChildSum = CurrentStep.ValidateChildSum(parentStep, currentNode, totalChildSum(leftChildSum, rightChildSum))
        NextStep.Continue(validateChildSum)
      }

      private def totalChildSum(leftChildSum: Option[BigInt], rightChildSum: Option[BigInt]): Option[BigInt] = {
        Option
          .and(leftChildSum, rightChildSum, _ + _)
          .orElse(leftChildSum)
          .orElse(rightChildSum)
      }
    }
  }


  private sealed trait CurrentStep {

    def next: NextStep
  }

  private object CurrentStep {

    def initial(root: SumTreeNode): CurrentStep = {
      val awaitingLeftChildOfRoot = ParentStep.AwaitingLeftChild(None, root)
      CurrentStep.VisitChild(awaitingLeftChildOfRoot, root, ChildToVisit.Left)
    }

    case class VisitChild(
      private val parentStep: ParentStep,
      private val currentNode: SumTreeNode,
      private val childToVisit: ChildToVisit,
    ) extends CurrentStep {

      override def next: NextStep = getChild match {
        case Some(childNode) =>
          val awaitingLeftChildOfChild = ParentStep.AwaitingLeftChild(Some(parentStep), childNode)
          val visitLeftChildOfChild = CurrentStep.VisitChild(awaitingLeftChildOfChild, childNode, ChildToVisit.Left)
          NextStep.Continue(visitLeftChildOfChild)

        case None => parentStep.provideSum(None)
      }

      private def getChild: Option[SumTreeNode] = childToVisit match {
        case ChildToVisit.Left => currentNode.leftChild
        case ChildToVisit.Right => currentNode.rightChild
      }
    }

    enum ChildToVisit {
      case Left, Right
    }

    case class ValidateChildSum(
      private val parentStep: Option[ParentStep],
      private val currentNode: SumTreeNode,
      private val totalChildSum: Option[BigInt],
    ) extends CurrentStep {

      override def next: NextStep = totalChildSum match {
        case Some(totalChildSum) =>
          if (currentNode.value != totalChildSum)
            NextStep.Yield(false)
          else
            provideSumToParent(currentNode.value + totalChildSum)

        case None => provideSumToParent(currentNode.value) // leaf nodes have no child sum -> nothing to validate, just provide value to parent
      }

      private def provideSumToParent(totalSum: BigInt): NextStep = parentStep match {
        case Some(parent) => parent.provideSum(Some(totalSum))
        case None => NextStep.Yield(true) // root node has no parent -> tree is valid sum tree
      }
    }
  }


  private enum NextStep {
    case Continue(next: CurrentStep)
    case Yield(result: Boolean)
  }
}
