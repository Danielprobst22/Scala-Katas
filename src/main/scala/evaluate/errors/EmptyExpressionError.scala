package evaluate.errors

case object EmptyExpressionError extends ParsingError {
  override def prettyErrorMessage(expression: String): String = {
    "Invalid expression - expression is empty"
  }
}
