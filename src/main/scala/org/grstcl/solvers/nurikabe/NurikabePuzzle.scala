package org.grstcl.solvers.nurikabe

import org.grstcl.solvers.{Rule, Puzzle}

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

object BlackConnected extends Rule[Unit, NurikabePuzzle, NurikabeGrid] with Connecting
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

object IslandsRule extends Rule[Unit, NurikabePuzzle, NurikabeGrid] with Connecting
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
