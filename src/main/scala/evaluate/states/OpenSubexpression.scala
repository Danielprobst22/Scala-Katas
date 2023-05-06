package evaluate.states

import evaluate.errors.{ParsingError, SyntaxError}
import evaluate.expressions.Expression
import evaluate.expressions.precedence1.Subexpression
import evaluate.{Index, Parenthesis, SymbolWithIndex}
import zio.prelude.NonEmptyList

case class OpenSubexpression private (
  private val openingParenthesisIndex: Index,
  private val onSubexpressionComplete: Subexpression => State,
  private val stateInSubexpression: State,
) extends State {

  override def newStateWithRemaining(remaining: NonEmptyList[SymbolWithIndex]): Either[ParsingError, (State, List[SymbolWithIndex])] = remaining.peel match {
    case ((Parenthesis.`)`, _), newRemaining) if noNestedSubexpression =>
      stateInSubexpression
        .onComplete
        .map(expression => Subexpression(expression))
        .map(subexpression => onSubexpressionComplete(subexpression))
        .map(newState => (newState, newRemaining))

    case _ =>
      stateInSubexpression
        .newStateWithRemaining(remaining)
        .map((newState, newRemaining) => (this.copy(stateInSubexpression = newState), newRemaining))
  }

  private def noNestedSubexpression: Boolean = {
    !stateInSubexpression.isInstanceOf[OpenSubexpression]
  }

  override def onComplete: Either[ParsingError, Expression] = {
    Left(SyntaxError("unmatched parenthesis", openingParenthesisIndex))
  }
}

object OpenSubexpression {

  def apply(openingParenthesisIndex: Index, onSubexpressionComplete: Subexpression => State): OpenSubexpression = {
    val stateInSubexpression = TermExpected(
      term => NonePending(term),
      SyntaxError("empty subexpression", openingParenthesisIndex),
    )

    new OpenSubexpression(openingParenthesisIndex, onSubexpressionComplete, stateInSubexpression)
  }
}
