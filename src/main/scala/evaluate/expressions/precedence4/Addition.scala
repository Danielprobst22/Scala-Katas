package evaluate.expressions.precedence4

import evaluate.expressions.precedence3.Precedence3AndHigher

case class Addition(leftAddend: Precedence4AndHigher, rightAddend: Precedence3AndHigher) extends Precedence4AndHigher {
  val value: BigDecimal = leftAddend.value + rightAddend.value
} 
