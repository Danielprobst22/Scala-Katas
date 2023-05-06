package evaluate.errors

trait ParsingError {
  def prettyErrorMessage(expression: String): String
}
