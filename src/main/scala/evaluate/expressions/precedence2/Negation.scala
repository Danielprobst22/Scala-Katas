package evaluate.expressions.precedence2

import evaluate.expressions.precedence1.Precedence1AndHigher

case class Negation(negated: Precedence1AndHigher) extends Precedence2AndHigher {
  val value: BigDecimal = -negated.value
} 
