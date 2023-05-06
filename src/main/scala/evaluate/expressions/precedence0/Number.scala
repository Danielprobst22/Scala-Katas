package evaluate.expressions.precedence0

import evaluate.*
import evaluate.errors.SyntaxError

import scala.annotation.tailrec

case class Number(value: BigDecimal) extends Precedence0

object Number {

  def parseAndGetRemaining(numericWithIndex: NumericWithIndex, remaining: List[SymbolWithIndex]): Either[SyntaxError, (Number, List[SymbolWithIndex])] = {
    @tailrec
    def iterate(integerPart: List[Digit], remaining: List[SymbolWithIndex]): Either[SyntaxError, (Number, List[SymbolWithIndex])] = remaining match {
      case (digit: Digit, _) :: newRemaining => iterate(integerPart.appended(digit), newRemaining)
      case (DecimalSeparator, _) :: (digit: Digit, _) :: newRemaining => parseFractionalPartAndGetRemaining(integerPart, digit, newRemaining)
      case (DecimalSeparator, index) :: _ => Left(SyntaxError("number must have at least one digit after the decimal separator", index))
      case newRemaining =>
        val number = Number(integerPartToBigDecimal(integerPart))
        Right((number, newRemaining))
    }

    (numericWithIndex, remaining) match {
      case ((DecimalSeparator, index), (_: Digit, _) :: _) => Left(SyntaxError("number must have at least one digit before the decimal separator", index))
      case ((DecimalSeparator, index), _) => Left(SyntaxError("decimal separator must be part of number", index))
      case ((Digit.`0`, index), (_: Digit, _) :: _) => Left(SyntaxError("multi-digit number must not start with 0", index))
      case ((digit: Digit, _), _) => iterate(List(digit), remaining)
    }
  }

  private def parseFractionalPartAndGetRemaining(integerPart: List[Digit], firstFractionalDigit: Digit, remaining: List[SymbolWithIndex]): Either[SyntaxError, (Number, List[SymbolWithIndex])] = {
    @tailrec
    def iterate(fractionalPart: List[Digit], remaining: List[SymbolWithIndex]): Either[SyntaxError, (Number, List[SymbolWithIndex])] = remaining match {
      case (digit: Digit, _) :: newRemaining => iterate(fractionalPart.appended(digit), newRemaining)
      case (DecimalSeparator, index) :: _ => Left(SyntaxError("number contains more than one decimal separator", index))
      case newRemaining =>
        val number = Number(integerPartToBigDecimal(integerPart) + fractionalPartToBigDecimal(fractionalPart))
        Right((number, newRemaining))
    }

    iterate(List(firstFractionalDigit), remaining)
  }

  private def integerPartToBigDecimal(integerPart: List[Digit]): BigDecimal = {
    integerPart
      .reverse
      .zipWithIndex
      .map((digit, position) => digit.value * BigDecimal(10).pow(position))
      .sum
  }

  private def fractionalPartToBigDecimal(fractionalPart: List[Digit]): BigDecimal = {
    fractionalPart
      .zipWithIndex
      .map((digit, position) => digit.value / BigDecimal(10).pow(position + 1))
      .sum
  }
}
