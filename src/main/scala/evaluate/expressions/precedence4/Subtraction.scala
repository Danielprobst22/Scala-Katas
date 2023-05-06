package evaluate.expressions.precedence4

import evaluate.expressions.precedence3.Precedence3AndHigher

case class Subtraction(minuend: Precedence4AndHigher, subtrahend: Precedence3AndHigher) extends Precedence4AndHigher {
  val value: BigDecimal = minuend.value - subtrahend.value
}
 
