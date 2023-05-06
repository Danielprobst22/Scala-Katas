package evaluate.expressions.precedence1

import evaluate.expressions.Expression

case class Subexpression(expression: Expression) extends Precedence1AndHigher {
  val value: BigDecimal = expression.value
} 
