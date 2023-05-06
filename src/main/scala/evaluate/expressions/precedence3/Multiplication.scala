package evaluate.expressions.precedence3

import evaluate.expressions.precedence2.Precedence2AndHigher

case class Multiplication(multiplier: Precedence3AndHigher, multiplicand: Precedence2AndHigher) extends Precedence3AndHigher {
  val value: BigDecimal = multiplier.value * multiplicand.value
} 
