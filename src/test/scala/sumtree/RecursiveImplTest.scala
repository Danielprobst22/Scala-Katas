package sumtree

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*
import sumtree.SumTreeTestUtil.*

class RecursiveImplTest extends AnyFunSuite with Matchers {

  test("should return true, when evaluating empty tree") {
    RecursiveImpl.isValidSumTree(EMPTY_TREE) shouldBe true
  }

  test("should return true, when evaluating valid tree") {
    forAll(VALID_TREES) { tree =>
      RecursiveImpl.isValidSumTree(tree) shouldBe true
    }
  }

  test("should return false, when evaluating invalid tree") {
    forAll(INVALID_TREES) { tree =>
      RecursiveImpl.isValidSumTree(tree) shouldBe false
    }
  }

  test("should throw StackOverflowError, when evaluating unbalanced tree with depth ten thousand") {
    forAll(UNBALANCED_TREES_WITH_DEPTH_TEN_THOUSAND) { tree =>
      assertThrows[StackOverflowError] {
        RecursiveImpl.isValidSumTree(tree)
      }
    }
  }
}
