package evaluate.expressions

import evaluate.errors.{EmptyExpressionError, ParsingError}
import evaluate.states.{NonePending, State, TermExpected}
import evaluate.{Symbol, SymbolWithIndex, Whitespace}
import zio.prelude.NonEmptyList

import scala.annotation.tailrec

trait Expression {
  def value: BigDecimal
}

object Expression {

  def parse(expression: String): Either[ParsingError, BigDecimal] = {
    val initialState = TermExpected(
      nextTerm => NonePending(nextTerm),
      EmptyExpressionError,
    )

    Symbol
      .parse(expression)
      .flatMap(symbolsWithIndexes => iterate(initialState, symbolsWithIndexes))
  }

  @tailrec
  private def iterate(state: State, remaining: List[SymbolWithIndex]): Either[ParsingError, BigDecimal] = {
    trimWhitespaces(remaining) match {
      case head :: tail =>
        state.newStateWithRemaining(NonEmptyList.fromIterable(head, tail)) match {
          case Right((newState, newRemaining)) => iterate(newState, newRemaining)
          case Left(parsingError) => Left(parsingError)
        }

      case Nil =>
        state
          .onComplete
          .map(_.value)
    }
  }

  private def trimWhitespaces(symbolsWithIndexes: List[SymbolWithIndex]): List[SymbolWithIndex] = {
    symbolsWithIndexes.dropWhile((symbol, _) => symbol == Whitespace)
  }
}
