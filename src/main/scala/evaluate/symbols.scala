package evaluate

import evaluate.errors.{IllegalSymbolError, SyntaxError}
import evaluate.expressions.Expression
import zio.prelude.{NonEmptyForEachOps, Validation}

sealed trait Symbol {
  def character: Char
  def misplacedSymbolMessage: String
}

object Symbol {

  def parse(expression: String): Either[IllegalSymbolError, List[SymbolWithIndex]] = {
    Validation.validateAll(parseAndValidate(expression))
      .toEither
      .left.map(indexes => IllegalSymbolError(indexes.toNonEmptyList))
  }

  private def parseAndValidate(expression: String): List[Validation[Index, SymbolWithIndex]] = {
    expression
      .toCharArray
      .toList
      .zipWithIndex
      .map(byCharacter)
  }

  private def byCharacter(character: Char, index: Index): Validation[Index, SymbolWithIndex] = {
    characterToSymbol.get(character) match {
      case Some(symbol) => Validation.succeed((symbol, index))
      case None => Validation.fail(index)
    }
  }

  private val characterToSymbol: Map[Char, Symbol] =
    (Numeric.values ++ NonNumeric.values)
      .map(symbol => (symbol.character, symbol))
      .toMap
}

//region Numeric
sealed trait Numeric extends Symbol

object Numeric {
  def values: Array[Numeric] = Digit.values :+ DecimalSeparator
}

enum Digit(val character: Char, val value: Int) extends Numeric {
  case `0` extends Digit('0', 0)
  case `1` extends Digit('1', 1)
  case `2` extends Digit('2', 2)
  case `3` extends Digit('3', 3)
  case `4` extends Digit('4', 4)
  case `5` extends Digit('5', 5)
  case `6` extends Digit('6', 6)
  case `7` extends Digit('7', 7)
  case `8` extends Digit('8', 8)
  case `9` extends Digit('9', 9)

  val misplacedSymbolMessage: String = "misplaced number"
}

case object DecimalSeparator extends Numeric {
  val character: Char = '.'
  val misplacedSymbolMessage: String = "misplaced decimal separator"
}
//endregion

//region NonNumeric
sealed trait NonNumeric extends Symbol

object NonNumeric {
  def values: Array[NonNumeric] = Parenthesis.values ++ Operator.values :+ Whitespace
}

enum Parenthesis(val character: Char) extends NonNumeric {
  case `(` extends Parenthesis('(')
  case `)` extends Parenthesis(')')

  val misplacedSymbolMessage: String = "misplaced parenthesis"
}

//noinspection NoTargetNameAnnotationForOperatorLikeDefinition
enum Operator(val character: Char) extends NonNumeric {
  case `+` extends Operator('+')
  case `-` extends Operator('-')
  case `*` extends Operator('*')
  case `/` extends Operator('/')

  val misplacedSymbolMessage: String = "misplaced operator"
}

case object Whitespace extends NonNumeric {
  val character: Char = ' '

  val misplacedSymbolMessage: String = "misplaced whitespace" // defined only to appease the compiler -> whitespaces are filtered out 
}
//endregion
