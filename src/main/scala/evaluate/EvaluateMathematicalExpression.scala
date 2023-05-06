package evaluate

import evaluate.expressions.Expression

import java.text.DecimalFormat
import scala.io.StdIn

/**
 * https://www.codewars.com/kata/52a78825cdfc2cfc87000005
 */
object EvaluateMathematicalExpression extends App {
  private val formatter = DecimalFormat("#0.###")

  while (true) {
    println("\nIntroduce a mathematical expression:")
    val expression = StdIn.readLine()

    Expression.parse(expression) match {
      case Right(result) => println(s"\nResult: ${formatter.format(result)}")
      case Left(parsingError) => println(parsingError.prettyErrorMessage(expression))
    }
  }
}
