package evaluate.states

import evaluate.SymbolWithIndex
import evaluate.errors.ParsingError
import evaluate.expressions.Expression
import zio.prelude.NonEmptyList

trait State {
  def newStateWithRemaining(remaining: NonEmptyList[SymbolWithIndex]): Either[ParsingError, (State, List[SymbolWithIndex])]
  def onComplete: Either[ParsingError, Expression]
}
