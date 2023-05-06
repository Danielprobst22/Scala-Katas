package evaluate.expressions.precedence0

import evaluate.*
import evaluate.Digit.*
import evaluate.Operator.`+`
import evaluate.errors.SyntaxError
import evaluate.expressions.precedence0.Number.parseAndGetRemaining
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class NumberSuite extends AnyFunSuite with Matchers with EitherValues {

  //region SyntaxErrors
  test("should return a SyntaxError, when number starts with decimal separator") {
    val (numericWithIndex, remaining) = withIndex(DecimalSeparator, `0`)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.left.value shouldBe SyntaxError("number must have at least one digit before the decimal separator", 0)
  }

  test("should return a SyntaxError, when decimal separator is not part of number") {
    val (numericWithIndex, remaining) = withIndex(DecimalSeparator, `+`, `0`)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.left.value shouldBe SyntaxError("decimal separator must be part of number", 0)
  }

  test("should return a SyntaxError, when multi-digit number starts with 0") {
    val (numericWithIndex, remaining) = withIndex(`0`, `1`)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.left.value shouldBe SyntaxError("multi-digit number must not start with 0", 0)
  }

  test("should return a SyntaxError, when number does not have at least one digit after the decimal separator") {
    val (numericWithIndex, remaining) = withIndex(`0`, DecimalSeparator)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.left.value shouldBe SyntaxError("number must have at least one digit after the decimal separator", 1)
  }

  test("should return a SyntaxError, when number contains more than one decimal separator") {
    val (numericWithIndex, remaining) = withIndex(`0`, DecimalSeparator, `1`, DecimalSeparator, `2`)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.left.value shouldBe SyntaxError("number contains more than one decimal separator", 3)
  }
  //endregion

  //region Valid Numbers
  test("should return a Number, when parsing one decimal digit") {
    val numericWithIndexAndRemainingToExpectedNumber = Table(
      ("numericWithIndex", "remaining", "expectedNumber"),
      withIndex(`0`) :* Number(BigDecimal(0)),
      withIndex(`1`) :* Number(BigDecimal(1)),
      withIndex(`2`) :* Number(BigDecimal(2)),
      withIndex(`3`) :* Number(BigDecimal(3)),
      withIndex(`4`) :* Number(BigDecimal(4)),
      withIndex(`5`) :* Number(BigDecimal(5)),
      withIndex(`6`) :* Number(BigDecimal(6)),
      withIndex(`7`) :* Number(BigDecimal(7)),
      withIndex(`8`) :* Number(BigDecimal(8)),
      withIndex(`9`) :* Number(BigDecimal(9)),
    )

    forAll(numericWithIndexAndRemainingToExpectedNumber) { (numericWithIndex, remaining, expectedNumber) =>
      val result = parseAndGetRemaining(numericWithIndex, remaining)

      result.value shouldBe (expectedNumber, List.empty)
    }
  }

  test("should return a Number, when parsing one decimal and one fractional digit") {
    val numericWithIndexAndRemainingToExpectedNumber = Table(
      ("numericWithIndex", "remaining", "expectedNumber"),
      withIndex(`0`, DecimalSeparator, `0`) :* Number(BigDecimal(0.0)),
      withIndex(`0`, DecimalSeparator, `1`) :* Number(BigDecimal(0.1)),
      withIndex(`0`, DecimalSeparator, `2`) :* Number(BigDecimal(0.2)),
      withIndex(`0`, DecimalSeparator, `3`) :* Number(BigDecimal(0.3)),
      withIndex(`0`, DecimalSeparator, `4`) :* Number(BigDecimal(0.4)),
      withIndex(`0`, DecimalSeparator, `5`) :* Number(BigDecimal(0.5)),
      withIndex(`0`, DecimalSeparator, `6`) :* Number(BigDecimal(0.6)),
      withIndex(`0`, DecimalSeparator, `7`) :* Number(BigDecimal(0.7)),
      withIndex(`0`, DecimalSeparator, `8`) :* Number(BigDecimal(0.8)),
      withIndex(`0`, DecimalSeparator, `9`) :* Number(BigDecimal(0.9)),
    )

    forAll(numericWithIndexAndRemainingToExpectedNumber) { (numericWithIndex, remaining, expectedNumber) =>
      val result = parseAndGetRemaining(numericWithIndex, remaining)

      result.value shouldBe (expectedNumber, List.empty)
    }
  }

  test("should return a Number, when parsing decimal and fractional digits with DecimalSeparator at varying positions") {
    val numericWithIndexAndRemainingToExpectedNumber = Table(
      ("numericWithIndex", "remaining", "expectedNumber"),
      withIndex(`1`, DecimalSeparator, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`) :* Number(BigDecimal(1.23456789)),
      withIndex(`1`, `2`, DecimalSeparator, `3`, `4`, `5`, `6`, `7`, `8`, `9`) :* Number(BigDecimal(12.3456789)),
      withIndex(`1`, `2`, `3`, DecimalSeparator, `4`, `5`, `6`, `7`, `8`, `9`) :* Number(BigDecimal(123.456789)),
      withIndex(`1`, `2`, `3`, `4`, DecimalSeparator, `5`, `6`, `7`, `8`, `9`) :* Number(BigDecimal(1234.56789)),
      withIndex(`1`, `2`, `3`, `4`, `5`, DecimalSeparator, `6`, `7`, `8`, `9`) :* Number(BigDecimal(12345.6789)),
      withIndex(`1`, `2`, `3`, `4`, `5`, `6`, DecimalSeparator, `7`, `8`, `9`) :* Number(BigDecimal(123456.789)),
      withIndex(`1`, `2`, `3`, `4`, `5`, `6`, `7`, DecimalSeparator, `8`, `9`) :* Number(BigDecimal(1234567.89)),
      withIndex(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, DecimalSeparator, `9`) :* Number(BigDecimal(12345678.9)),
      withIndex(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `0`) :* Number(BigDecimal(1234567890)),
    )

    forAll(numericWithIndexAndRemainingToExpectedNumber) { (numericWithIndex, remaining, expectedNumber) =>
      val result = parseAndGetRemaining(numericWithIndex, remaining)

      result.value shouldBe (expectedNumber, List.empty)
    }
  }

  test("should return a Number and the remaining symbols, when parsing decimal and fractional digits plus other symbols") {
    val (numericWithIndex, remaining) = withIndex(`0`, DecimalSeparator, `1`, `+`, `2`)

    val result = parseAndGetRemaining(numericWithIndex, remaining)

    result.value shouldBe (Number(BigDecimal(0.1)), List((`+`, 3), (`2`, 4)))
  }
  //endregion

  private def withIndex(numeric: Numeric, symbols: Symbol*): (NumericWithIndex, List[SymbolWithIndex]) = {
    val symbolsWithIndexes = symbols
      .toList
      .zipWithIndex
      .map((symbol, index) => (symbol, index + 1))

    ((numeric, 0), symbolsWithIndexes)
  }
}
