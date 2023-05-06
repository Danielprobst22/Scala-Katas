package evaluate.errors

import evaluate.Index
import zio.prelude.NonEmptyList

import scala.annotation.tailrec

/**
 * @param indexes indexes of one or more characters at which an illegal symbol was found - each index should start at 0
 */
case class IllegalSymbolError(indexes: NonEmptyList[Index]) extends ParsingError {
  override def prettyErrorMessage(expression: String): String = {
    s"""
       |Invalid expression - illegal $singularOrPlural:
       |$expression
       |$markers""".stripMargin
  }

  private def singularOrPlural: String = {
    if (indexes.length == 1)
      "symbol"
    else
      "symbols"
  }

  private def markers: String = {
    @tailrec
    def iterate(lastMarkerIndex: Index, remaining: List[Index], builder: StringBuilder): String = remaining match {
      case index :: newRemaining =>
        val numberOfWhitespaces = index - lastMarkerIndex
        builder
          .append(nWhitespaces(numberOfWhitespaces))
          .append("^")
        iterate(index + 1, newRemaining, builder)

      case Nil => builder.toString()
    }

    def nWhitespaces(n: Int): String = " " * n

    indexes.peel match {
      case (index, remaining) =>
        val builder = StringBuilder(nWhitespaces(index)).append("^")
        iterate(index + 1, remaining, builder)
    }
  }
}
