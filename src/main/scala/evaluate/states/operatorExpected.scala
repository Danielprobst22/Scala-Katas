package evaluate.states

import evaluate.errors.{ParsingError, SyntaxError}
import evaluate.expressions.Expression
import evaluate.expressions.precedence2.Precedence2AndHigher
import evaluate.expressions.precedence3.{Division, Multiplication, Precedence3AndHigher}
import evaluate.expressions.precedence4.{Addition, Precedence4AndHigher, Subtraction}
import evaluate.{Operator, Parenthesis, SymbolWithIndex}
import zio.prelude.NonEmptyList

case class NonePending(private val bufferedTerm: Precedence3AndHigher) extends State {

  override def newStateWithRemaining(remaining: NonEmptyList[SymbolWithIndex]): Either[ParsingError, (State, List[SymbolWithIndex])] = {
    def ifNextTermFound(operator: Operator): Precedence2AndHigher => State = operator match {
      case Operator.`+` => nextTerm => AdditionOrSubtractionPending(bufferedTerm, nextTerm, (l, r) => Addition(l, r))
      case Operator.`-` => nextTerm => AdditionOrSubtractionPending(bufferedTerm, nextTerm, (l, r) => Subtraction(l, r))
      case Operator.`*` => nextTerm => NonePending(Multiplication(bufferedTerm, nextTerm))
      case Operator.`/` => nextTerm => NonePending(Division(bufferedTerm, nextTerm))
    }

    operatorExpected(remaining, ifNextTermFound)
  }

  override def onComplete: Either[ParsingError, Expression] = {
    Right(bufferedTerm)
  }
}


case class AdditionOrSubtractionPending(
  private val bufferedLeftTerm: Precedence4AndHigher,
  private val bufferedRightTerm: Precedence3AndHigher,
  private val pendingOperation: (Precedence4AndHigher, Precedence3AndHigher) => Precedence4AndHigher,
) extends State {

  override def newStateWithRemaining(remaining: NonEmptyList[SymbolWithIndex]): Either[ParsingError, (State, List[SymbolWithIndex])] = {
    def ifNextTermFound(operator: Operator): Precedence2AndHigher => State = operator match {
      case Operator.`+` => nextTerm => AdditionOrSubtractionPending(resultOfPendingOperation, nextTerm, (l, r) => Addition(l, r))
      case Operator.`-` => nextTerm => AdditionOrSubtractionPending(resultOfPendingOperation, nextTerm, (l, r) => Subtraction(l, r))
      case Operator.`*` => nextTerm => this.copy(bufferedRightTerm = Multiplication(bufferedRightTerm, nextTerm))
      case Operator.`/` => nextTerm => this.copy(bufferedRightTerm = Division(bufferedRightTerm, nextTerm))
    }

    def resultOfPendingOperation: Precedence4AndHigher = pendingOperation(bufferedLeftTerm, bufferedRightTerm)

    operatorExpected(remaining, ifNextTermFound)
  }

  override def onComplete: Either[ParsingError, Expression] = {
    Right(pendingOperation(bufferedLeftTerm, bufferedRightTerm))
  }
}


private def operatorExpected(
  remaining: NonEmptyList[SymbolWithIndex],
  ifNextTermFound: Operator => Precedence2AndHigher => State
): Either[SyntaxError, (TermExpected, List[SymbolWithIndex])] = remaining.peel match {
  case ((operator: Operator, index), newRemaining) =>
    val ifNextTermNotFound = SyntaxError(s"expected a number or subexpression after operator", index)
    val newState = TermExpected(ifNextTermFound(operator), ifNextTermNotFound)
    Right((newState, newRemaining))

  case ((Parenthesis.`)`, index), _) =>
    Left(SyntaxError("unmatched parenthesis", index))

  case ((misplacedSymbol, index), _) =>
    Left(SyntaxError(s"${misplacedSymbol.misplacedSymbolMessage} - expected an operator", index))
}
