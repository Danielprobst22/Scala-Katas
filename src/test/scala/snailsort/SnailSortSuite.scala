package snailsort

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*
import snailsort.SnailSort.snailSort

class SnailSortSuite extends AnyFunSuite with Matchers with EitherValues {

  test("should return IllegalArgumentException, when rows have different number of columns") {
    val values = Array(
      Array(1, 2),
      Array(3),
    )

    val result = snailSort(values)

    result.left.value shouldBe an [IllegalArgumentException]
  }

  test("should return empty vector, when rows are empty") {
    val values = Array.empty[Array[Int]]

    val result = snailSort(values)

    result.value shouldBe Vector.empty
  }

  test("should return empty vector, when columns are empty") {
    val values = Array(Array.empty[Int])

    val result = snailSort(values)

    result.value shouldBe Vector.empty
  }

  test("should return vector, when values have dimension 1x1") {
    val values = Array(
      Array(1),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1)
  }

  test("should return vector, when values have dimension 1x2") {
    val values = Array(
      Array(1, 2),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2)
  }

  test("should return vector, when values have dimension 2x1") {
    val values = Array(
      Array(1),
      Array(2),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2)
  }

  test("should return vector, when values have dimension 2x2") {
    val values = Array(
      Array(1, 2),
      Array(4, 3),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4)
  }

  test("should return vector, when values have dimension 2x3") {
    val values = Array(
      Array(1, 2, 3),
      Array(6, 5, 4),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6)
  }

  test("should return vector, when values have dimension 3x2") {
    val values = Array(
      Array(1, 2),
      Array(6, 3),
      Array(5, 4),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6)
  }

  test("should return vector, when values have dimension 3x3") {
    val values = Array(
      Array(1, 2, 3),
      Array(8, 9, 4),
      Array(7, 6, 5),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("should return vector, when values have dimension 3x4") {
    val values = Array(
      Array(1, 2, 3, 4),
      Array(10, 11, 12, 5),
      Array(9, 8, 7, 6),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  test("should return vector, when values have dimension 4x3") {
    val values = Array(
      Array(1, 2, 3),
      Array(10, 11, 4),
      Array(9, 12, 5),
      Array(8, 7, 6),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  test("should return vector, when values have dimension 4x4") {
    val values = Array(
      Array(1, 2, 3, 4),
      Array(12, 13, 14, 5),
      Array(11, 16, 15, 6),
      Array(10, 9, 8, 7),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  }

  test("should return vector, when values have dimension 5x5") {
    val values = Array(
      Array(1, 2, 3, 4, 5),
      Array(16, 17, 18, 19, 6),
      Array(15, 24, 25, 20, 7),
      Array(14, 23, 22, 21, 8),
      Array(13, 12, 11, 10, 9),
    )

    val result = snailSort(values)

    result.value shouldBe Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
  }
}
