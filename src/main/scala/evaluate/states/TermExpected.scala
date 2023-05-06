package evaluate.states

import evaluate.errors.{ParsingError, SyntaxError}
import evaluate.expressions.Expression
import evaluate.expressions.precedence0.Number
import evaluate.expressions.precedence1.Subexpression
import evaluate.expressions.precedence2.{Negation, Precedence2AndHigher}
import evaluate.{Index, Numeric, Operator, Parenthesis, SymbolWithIndex, Whitespace}
import zio.prelude.NonEmptyList

case class TermExpected(
  private val onNextTermFound: Precedence2AndHigher => State,
  private val ifNextTermNotFound: ParsingError,
) extends State {

  override def newStateWithRemaining(remaining: NonEmptyList[SymbolWithIndex]): Either[ParsingError, (State, List[SymbolWithIndex])] = remaining.peel match {
    case ((numeric: Numeric, index), newRemaining) =>
      Number
        .parseAndGetRemaining((numeric, index), newRemaining)
        .map((number, remaining) => (onNextTermFound(number), remaining))

    case ((Operator.`-`, _), (numeric: Numeric, index) :: newRemaining) =>
      Number
        .parseAndGetRemaining((numeric, index), newRemaining)
        .map((number, remaining) => (onNextTermFound(Negation(number)), remaining))

    case ((Parenthesis.`(`, index), newRemaining) =>
      val newState = OpenSubexpression(index, subexpression => onNextTermFound(subexpression))
      Right(newState, newRemaining)

    case ((Operator.`-`, _), (Parenthesis.`(`, index) :: newRemaining) =>
      val newState = OpenSubexpression(index, subexpression => onNextTermFound(Negation(subexpression)))
      Right(newState, newRemaining)

    case ((Operator.`-`, _), (Whitespace, index) :: (_: Numeric | Parenthesis.`(`, _) :: _) =>
      Left(SyntaxError("whitespace not allowed between negation sign and negated term", index))

    case ((misplacedSymbol, index), _) =>
      Left(SyntaxError(s"${misplacedSymbol.misplacedSymbolMessage} - expected a number or subexpression", index))
  }

  override def onComplete: Either[ParsingError, Expression] = {
    Left(ifNextTermNotFound)
  }
}
