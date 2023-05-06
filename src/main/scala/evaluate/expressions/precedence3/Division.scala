package evaluate.expressions.precedence3

import evaluate.expressions.precedence2.Precedence2AndHigher

case class Division(dividend: Precedence3AndHigher, divisor: Precedence2AndHigher) extends Precedence3AndHigher {
  val value: BigDecimal = dividend.value / divisor.value
} 
