import Direction.{MoveRight, nextPositionInDirection, nextPositionWithNextDirectionAndNewLimits}

import scala.annotation.tailrec

/**
 * https://dev.to/thepracticaldev/daily-challenge-59-snail-sort-1a6n
 */
object SnailSort {

  def snailSort(values: Array[Array[Int]]): Either[IllegalArgumentException, Vector[Int]] = {
    @tailrec
    def iterate(position: Position, direction: Direction, limits: Limits, accumulated: Vector[Int]): Vector[Int] = {
      val result = accumulated.appended(values(position.row)(position.column))
      nextPositionInDirection(position, direction, limits) match {
        case Some(nextPosition) => iterate(nextPosition, direction, limits, result)
        case None =>
          nextPositionWithNextDirectionAndNewLimits(position, direction, limits) match {
            case Some(nextPosition, nextDirection, newLimits) => iterate(nextPosition, nextDirection, newLimits, result)
            case None => result
          }
      }
    }

    if (values.isEmpty || values(0).isEmpty) {
      return Right(Vector.empty)
    }

    val numberOfRows = values.length
    val numberOfColumns = values(0).length

    val rowLengths = values.map(_.length)
    if (!rowLengths.forall(_ == numberOfColumns)) {
      return Left(IllegalArgumentException("All rows must have the same number of columns"))
    }

    val result = iterate(
      Position(0, 0),
      MoveRight,
      Limits(numberOfColumns - 1, numberOfRows - 1, 0, 0),
      Vector.empty,
    )
    Right(result)
  }
}

case class Position(
  row: Int,
  column: Int,
) {
  def moveRight: Position = this.copy(column = column + 1)

  def moveDown: Position = this.copy(row = row + 1)

  def moveLeft: Position = this.copy(column = column - 1)

  def moveUp: Position = this.copy(row = row - 1)
}

case class Limits(
  right: Int,
  bottom: Int,
  left: Int,
  top: Int,
) {
  def shrinkRight: Limits = this.copy(right = right - 1)

  def shrinkBottom: Limits = this.copy(bottom = bottom - 1)

  def shrinkLeft: Limits = this.copy(left = left + 1)

  def shrinkTop: Limits = this.copy(top = top + 1)
}

enum Direction {
  case MoveRight
  case MoveDown
  case MoveLeft
  case MoveUp
}

object Direction {
  def nextPositionInDirection(position: Position, direction: Direction, limits: Limits): Option[Position] = direction match {
    case MoveRight => Option.when(position.column < limits.right)(position.moveRight)
    case MoveDown => Option.when(position.row < limits.bottom)(position.moveDown)
    case MoveLeft => Option.when(position.column > limits.left)(position.moveLeft)
    case MoveUp => Option.when(position.row > limits.top)(position.moveUp)
  }

  def nextPositionWithNextDirectionAndNewLimits(position: Position, direction: Direction, limits: Limits): Option[(Position, Direction, Limits)] = {
    val (nextDirection, newLimits) = nextDirectionWithNewLimits(direction, limits)
    nextPositionInDirection(position, nextDirection, newLimits)
      .map(nextPosition => (nextPosition, nextDirection, newLimits))
  }

  private def nextDirectionWithNewLimits(direction: Direction, limits: Limits): (Direction, Limits) = direction match {
    case MoveRight => (MoveDown, limits.shrinkTop)
    case MoveDown => (MoveLeft, limits.shrinkRight)
    case MoveLeft => (MoveUp, limits.shrinkBottom)
    case MoveUp => (MoveRight, limits.shrinkLeft)
  }
}
