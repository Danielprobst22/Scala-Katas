package sumtree

import ox.either
import ox.either.*

import scala.util.boundary.break

object RecursiveImpl {

  def isValidSumTree(root: SumTreeNode): Boolean = calculateSumRecursively(root).isRight

  private def calculateSumRecursively(currentNode: SumTreeNode): Either[IllegalArgumentException, BigInt] = {
    either {
      if (currentNode.leftChild.isEmpty && currentNode.rightChild.isEmpty) {
        return Right(currentNode.value) // early return for leaves
      }

      val leftChildSum = currentNode.leftChild match {
        case Some(leftChild) => calculateSumRecursively(leftChild).ok()
        case None => BigInt(0)
      }

      val rightChildSum = currentNode.rightChild match {
        case Some(rightChild) => calculateSumRecursively(rightChild).ok()
        case None => BigInt(0)
      }

      val totalChildSum = leftChildSum + rightChildSum
      if (currentNode.value != totalChildSum)
        IllegalArgumentException(s"Node has value = ${currentNode.value}, but children have sum = $totalChildSum").fail()
      else
        currentNode.value + totalChildSum
    }
  }
}
