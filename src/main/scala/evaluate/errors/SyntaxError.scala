package evaluate.errors

import evaluate.Index

/**
 * @param message Error message - should end without any punctuation, as [[prettyErrorMessage]] appends a ':' at the end
 * @param index   index of the character at which the syntax error was found - should start at 0
 */
case class SyntaxError(message: String, index: Index) extends ParsingError {
  override def prettyErrorMessage(expression: String): String = {
    s"""
       |Invalid expression - $message:
       |$expression
       |$marker""".stripMargin
  }

  private def marker: String = " " * index + "^"
}
