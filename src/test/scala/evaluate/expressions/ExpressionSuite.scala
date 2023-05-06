package evaluate.expressions

import evaluate.errors.{EmptyExpressionError, IllegalSymbolError, SyntaxError}
import evaluate.expressions.Expression.parse
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class ExpressionSuite extends AnyFunSuite with Matchers with EitherValues {

  //region SyntaxErrors
  test("should return an EmptyExpressionError, when expression is empty") {
    val expressions = Table(
      "expression",
      "",
      " ",
      "   ",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe EmptyExpressionError
    }
  }

  test("should return an IllegalSymbolError, when expression contains illegal symbols") {
    val expressions = Table(
      "expression",
      "42 x 7",
      "42 % 7",
      "42^7",
      "test",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[IllegalSymbolError]
    }
  }

  test("should return a SyntaxError, when expression does not start with a number or subexpression") {
    val expressions = Table(
      "expression",
      "+",
      "+ 42",
      "-",
      "- 42",
      "*",
      "* 42",
      "/",
      "/ 42",
      ")",
      ") 42",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }

  test("should return a SyntaxError, when number is not followed by an operator") {
    val expressions = Table(
      "expression",
      "42 (",
      "42 )",
      "42 .",
      "42 42",
      "42 (42)",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }

  test("should return a SyntaxError, when operator is not followed by a number or subexpression") {
    val expressions = Table(
      "expression",
      "42 +",
      "42 + +",
      "42 -",
      "42 - -",
      "42 *",
      "42 * *",
      "42 /",
      "42 / /",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }

  test("should return a SyntaxError, when a whitespace is present between negation sign and negated term") {
    val expressions = Table(
      "expression",
      "- 42",
      "- (42)",
      "42 + - 42",
      "42 - - 42",
      "42 * - 42",
      "42 / - 42",
      "42 + - (42)",
      "42 - - (42)",
      "42 * - (42)",
      "42 / - (42)",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }

  test("should return a SyntaxError, when parenthesis is unmatched") {
    val expressions = Table(
      "expression",
      "(",
      ")",
      "((",
      "))",

      "(42",
      "42)",
      "((42",
      "42))",
      "((42)",
      "(42))",
      "(((42))",
      "((42)))",

      "(42 + 42",
      "(42 - 42",
      "(42 * 42",
      "(42 / 42",

      "42 + (42",
      "42 - (42",
      "42 * (42",
      "42 / (42",

      "42) + 42",
      "42) - 42",
      "42) * 42",
      "42) / 42",

      "42 + 42)",
      "42 - 42)",
      "42 * 42)",
      "42 / 42)",

      "(42) + (42",
      "(42) - (42",
      "(42) * (42",
      "(42) / (42",

      "(42 + (42)",
      "(42 - (42)",
      "(42 * (42)",
      "(42 / (42)",

      "((42) + 42",
      "((42) - 42",
      "((42) * 42",
      "((42) / 42",

      "42 + ((42)",
      "42 - ((42)",
      "42 * ((42)",
      "42 / ((42)",

      "((42 + 42)",
      "((42 - 42)",
      "((42 * 42)",
      "((42 / 42)",

      "(42 + 42))",
      "(42 - 42))",
      "(42 * 42))",
      "(42 / 42))",

      "(42 + 42 + 42",
      "(42 - 42 - 42",
      "(42 * 42 * 42",
      "(42 / 42 / 42",

      "42 + (42 + 42",
      "42 - (42 - 42",
      "42 * (42 * 42",
      "42 / (42 / 42",

      "42 + 42 + (42",
      "42 - 42 - (42",
      "42 * 42 * (42",
      "42 / 42 / (42",

      "((42 + 42) + 42",
      "((42 - 42) - 42",
      "((42 * 42) * 42",
      "((42 / 42) / 42",

      "(42 + (42 + 42)",
      "(42 - (42 - 42)",
      "(42 * (42 * 42)",
      "(42 / (42 / 42)",

      "((42 + 42 + 42)",
      "((42 - 42 - 42)",
      "((42 * 42 * 42)",
      "((42 / 42 / 42)",

      "(42 + 42 + 42))",
      "(42 - 42 - 42))",
      "(42 * 42 * 42))",
      "(42 / 42 / 42))",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }

  test("should return a SyntaxError, when number is misformatted") {
    val expressions = Table(
      "expression",
      ".42",
      "42.",
      "42.42.42",
      "042",
    )

    forAll(expressions) { expression =>
      val result = parse(expression)

      result.left.value shouldBe an[SyntaxError]
    }
  }
  //endregion

  //region Valid Expressions
  test("should return result, when parsing a single number") {
    val expressionToExpectedResult = Table(
      ("expression", "expectedResult"),
      ("42", BigDecimal(42)),
      ("42.42", BigDecimal(42.42)),
    )

    forAll(expressionToExpectedResult) { (expression, expectedResult) =>
      val result = parse(expression)

      result.value shouldBe expectedResult
    }
  }

  test("should return result, when parsing a single operation") {
    val expressionToExpectedResult = Table(
      ("expression", "expectedResult"),
      ("1 + 2", BigDecimal(3)),
      ("2 + 1", BigDecimal(3)),

      ("1 - 2", BigDecimal(-1)),
      ("2 - 1", BigDecimal(1)),

      ("1 * 2", BigDecimal(2)),
      ("2 * 1", BigDecimal(2)),

      ("1 / 2", BigDecimal(0.5)),
      ("2 / 1", BigDecimal(2)),
    )

    forAll(expressionToExpectedResult) { (expression, expectedResult) =>
      val result = parse(expression)

      result.value shouldBe expectedResult
    }
  }

  test("should return result, when parsing two operations") {
    val expressionToExpectedResult = Table(
      ("expression", "expectedResult"),
      ("3 + 4 + 5", BigDecimal(12)),
      ("3 + 4 - 5", BigDecimal(2)),
      ("3 + 4 * 5", BigDecimal(23)),
      ("3 + 4 / 5", BigDecimal(3.8)),

      ("3 - 4 + 5", BigDecimal(4)),
      ("3 - 4 - 5", BigDecimal(-6)),
      ("3 - 4 * 5", BigDecimal(-17)),
      ("3 - 4 / 5", BigDecimal(2.2)),

      ("3 * 4 + 5", BigDecimal(17)),
      ("3 * 4 - 5", BigDecimal(7)),
      ("3 * 4 * 5", BigDecimal(60)),
      ("3 * 4 / 5", BigDecimal(2.4)),

      ("3 / 4 + 5", BigDecimal(5.75)),
      ("3 / 4 - 5", BigDecimal(-4.25)),
      ("3 / 4 * 5", BigDecimal(3.75)),
      ("3 / 4 / 5", BigDecimal(0.15)),
    )

    forAll(expressionToExpectedResult) { (expression, expectedResult) =>
      val result = parse(expression)

      result.value shouldBe expectedResult
    }
  }

  test("should return result, when parsing operations with subexpressions") {
    val expressionToExpectedResult = Table(
      ("expression", "expectedResult"),
      ("(42)", BigDecimal(42)),
      ("((42))", BigDecimal(42)),
      ("((42))", BigDecimal(42)),
      ("(((42)))", BigDecimal(42)),

      ("(1 + 2)", BigDecimal(3)),
      ("(1 - 2)", BigDecimal(-1)),
      ("(1 * 2)", BigDecimal(2)),
      ("(1 / 2)", BigDecimal(0.5)),

      ("3 + (4 + 5)", BigDecimal(12)),
      ("(3 + 4) + 5", BigDecimal(12)),
      ("3 + (4 - 5)", BigDecimal(2)),
      ("(3 + 4) - 5", BigDecimal(2)),
      ("3 + (4 * 5)", BigDecimal(23)),
      ("(3 + 4) * 5", BigDecimal(35)),
      ("3 + (4 / 5)", BigDecimal(3.8)),
      ("(3 + 4) / 5", BigDecimal(1.4)),

      ("3 - (4 + 5)", BigDecimal(-6)),
      ("(3 - 4) + 5", BigDecimal(4)),
      ("3 - (4 - 5)", BigDecimal(4)),
      ("(3 - 4) - 5", BigDecimal(-6)),
      ("3 - (4 * 5)", BigDecimal(-17)),
      ("(3 - 4) * 5", BigDecimal(-5)),
      ("3 - (4 / 5)", BigDecimal(2.2)),
      ("(3 - 4) / 5", BigDecimal(-0.2)),

      ("3 * (4 + 5)", BigDecimal(27)),
      ("(3 * 4) + 5", BigDecimal(17)),
      ("3 * (4 - 5)", BigDecimal(-3)),
      ("(3 * 4) - 5", BigDecimal(7)),
      ("3 * (4 * 5)", BigDecimal(60)),
      ("(3 * 4) * 5", BigDecimal(60)),
      ("3 * (4 / 5)", BigDecimal(2.4)),
      ("(3 * 4) / 5", BigDecimal(2.4)),

      ("3 / (4 + 5)", BigDecimal("0.3333333333333333333333333333333333")),
      ("(3 / 4) + 5", BigDecimal(5.75)),
      ("3 / (4 - 5)", BigDecimal(-3)),
      ("(3 / 4) - 5", BigDecimal(-4.25)),
      ("3 / (4 * 5)", BigDecimal(0.15)),
      ("(3 / 4) * 5", BigDecimal(3.75)),
      ("3 / (4 / 5)", BigDecimal(3.75)),
      ("(3 / 4) / 5", BigDecimal(0.15)),

      ("(1 - 2) + 3 * 4", BigDecimal(11)),
      ("1 - (2 + 3) * 4", BigDecimal(-19)),
      ("((1 - 2) + 3) * 4", BigDecimal(8)),
      ("(1 - (2 + 3)) * 4", BigDecimal(-16)),
    )

    forAll(expressionToExpectedResult) { (expression, expectedResult) =>
      val result = parse(expression)

      result.value shouldBe expectedResult
    }
  }

  test("should return result, when parsing negated terms") {
    val expressionToExpectedResult = Table(
      ("expression", "expectedResult"),
      ("-42", BigDecimal(-42)),
      ("-(42)", BigDecimal(-42)),
      ("-(-42)", BigDecimal(42)),
      ("-(-(-42))", BigDecimal(-42)),

      ("-1 + 2", BigDecimal(1)),
      ("1 + -2", BigDecimal(-1)),
      ("-1 + -2", BigDecimal(-3)),
      
      ("-1 - 2", BigDecimal(-3)),
      ("1 - -2", BigDecimal(3)),
      ("-1 - -2", BigDecimal(1)),
      
      ("-1 * 2", BigDecimal(-2)),
      ("1 * -2", BigDecimal(-2)),
      ("-1 * -2", BigDecimal(2)),
      
      ("-1 / 2", BigDecimal(-0.5)),
      ("1 / -2", BigDecimal(-0.5)),
      ("-1 / -2", BigDecimal(0.5)),
    )

    forAll(expressionToExpectedResult) { (expression, expectedResult) =>
      val result = parse(expression)

      result.value shouldBe expectedResult
    }
  }
  //endregion
}
