package org.grstcl.solvers.nurikabe

import org.grstcl.solvers.{Rule, Puzzle}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object No2x2Black extends Rule[Unit, NurikabePuzzle, NurikabeGrid]
{
  override def apply(grid: NurikabeGrid): Boolean =
  {
    val squares = (0 until (grid.puzzle.numRows - 1)).flatMap
    {
      row =>
        (0 until (grid.puzzle.numCols - 1)).map
        {
          col => Iterable((row, col), (row + 1, col), (row, col + 1), (row + 1, col + 1))
        }
    }

    squares.forall
    {
      square =>
        square.flatMap
        {
          pos =>
            grid.get(pos)
        }.count(_.isEmpty) != 4
    }
  }
}

abstract class ConnectingRule extends Rule[Unit, NurikabePuzzle, NurikabeGrid]
{
  def getConnectedThings(pos: (Int, Int), grid: NurikabeGrid): Set[(Int, Int)] =
  {
    val connectionType = grid.get(pos)

    if (connectionType.isEmpty)
    {
      Set.empty[(Int, Int)]
    }
    else
    {
      val test =
      {
        if (connectionType.get.isEmpty)
        {
          v: Option[Unit] => v.isEmpty
        }
        else
        {
          v: Option[Unit] => v.nonEmpty
        }
      }

      @tailrec
      def getConnectedSquares(testSquares: Queue[(Int, Int)], connectedSquares: Set[(Int, Int)]): Set[(Int, Int)] =
      {
        if (testSquares.isEmpty)
        {
          connectedSquares
        }
        else
        {
          val (testRow, testCol) = testSquares.head

          val newSquares = Iterable((testRow - 1, testCol),
            (testRow + 1, testCol),
            (testRow, testCol - 1),
            (testRow, testCol + 1)).filter
          {
            case (r, c) => !connectedSquares.contains((r, c)) &&
              grid.get(r, c).nonEmpty &&
              test(grid.get(r, c).get)
          }

          getConnectedSquares(testSquares.drop(1) ++ newSquares, connectedSquares ++ Set((testRow, testCol)) ++ newSquares)
        }
      }

      getConnectedSquares(Queue(pos), Set.empty[(Int, Int)])
    }
  }
}

object BlackConnected extends ConnectingRule
{
  override def apply(grid: NurikabeGrid): Boolean =
  {
    val blackSquares = grid.filter
    {
      case ((_, _), value) => value.isEmpty
    }

    val firstSquare = blackSquares.headOption

    if (firstSquare.isEmpty)
    {
      true
    }
    else
    {
      val firstConnectedRegion = getConnectedThings(firstSquare.get._1, grid)

      val remainingSquares = blackSquares.filterNot{blackSquare => firstConnectedRegion.contains(blackSquare._1)}
      remainingSquares.isEmpty
    }
  }
}

object IslandsRule extends ConnectingRule
{
  override def apply(grid: NurikabeGrid): Boolean =
  {
    val validIslands = grid.puzzle.clues.map
    {
      case ((row, col), value) =>
        val island = getConnectedThings((row, col), grid)
        (island, value)
    }

    val rightSizes = validIslands.forall
    {
      case (island, value) => island.size == value
    }

    val noOverlap = validIslands.forall
    {
      case (island, _) => grid.puzzle.clues.map(_._1).count
      {
        case (row, col) => island.contains((row, col))
      } == 1
    }

    val noCluelessIslands = grid.forall
    {
      case ((row, col), value) =>
        value.isEmpty ||
        validIslands.flatMap(_._1).toSet.contains((row, col))
    }

    rightSizes && noOverlap && noCluelessIslands
  }
}

class NurikabePuzzle(override val numRows: Int,
                     override val numCols: Int,
                     val clues: Iterable[((Int, Int), Int)])
  extends Puzzle[Unit, NurikabePuzzle]
{
  override type G = NurikabeGrid

  override def rules = Iterable(No2x2Black, BlackConnected, IslandsRule)
  override def possibleValues = Iterable(Unit)
}
