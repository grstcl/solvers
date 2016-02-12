package org.grstcl.solvers.starbattle

import org.grstcl.solvers.Rule
import org.grstcl.solvers.Puzzle
import org.grstcl.solvers.PuzzleGrid

object StarAdjacency extends Rule[Unit, StarBattlePuzzle]
{
  override def apply(grid: PuzzleGrid[Unit, StarBattlePuzzle]): Boolean =
  {
    val starLocs = grid.flatMap
    {
      case ((r, c), v) => v.map{_ => (r, c)}
    }

    starLocs.forall
    {
      case (row, col) =>
        ((row - 1) to (row + 1)).flatMap
	{
	  r =>
	    ((col - 1) to (col + 1)).map
	    {
	      c => (r, c)
	    }
	}.flatMap
	{
	  case (r, c) => grid.get(r, c)
	}.flatten.size == 1
    }
  }
}

object RowsAndCols extends Rule[Unit, StarBattlePuzzle]
{
  def apply(grid: PuzzleGrid[Unit, StarBattlePuzzle]): Boolean =
  {
    val rows = (0 until grid.puzzle.numRows).forall
    {
      row =>
        val rowVals = (0 until grid.puzzle.numCols).map{c => (row, c)}
	   	 		.flatMap
	{
	  case (r, c) => grid.get(r, c)
	}

	(rowVals.flatten.size <= grid.puzzle.numStars) &&
	(rowVals.count(_.isEmpty) <= grid.puzzle.numCols - grid.puzzle.numStars)
    }

    val cols = (0 until grid.puzzle.numCols).forall
    {
      col =>
        val colVals = (0 until grid.puzzle.numRows).map{r => (r, col)}
	   	 		.flatMap
	{
	  case (r, c) => grid.get(r, c)
	}

	(colVals.flatten.size <= grid.puzzle.numStars) &&
	(colVals.count(_.isEmpty) <= grid.puzzle.numCols - grid.puzzle.numStars)
    }

    rows && cols
  }
}

object Regions extends Rule[Unit, StarBattlePuzzle]
{
  override def apply(grid: PuzzleGrid[Unit, StarBattlePuzzle]): Boolean =
  {
    grid.puzzle.regions.forall
    {
      region =>
        val regionVals = region.flatMap
	{
	  case (r, c) => grid.get(r, c)
	}

	(regionVals.flatten.size <= grid.puzzle.numStars) &&
	(regionVals.count(_.isEmpty) <= region.size - grid.puzzle.numStars)
    }
  }
}

case class StarBattlePuzzle(numStars: Int, numRows: Int, numCols: Int, regions: Iterable[Iterable[(Int, Int)]]) extends Puzzle[Unit, StarBattlePuzzle]
{
  def this(numStars: Int, dim: Int, regions: Iterable[Iterable[(Int, Int)]]) =
    this(numStars, dim, dim, regions)

  override def rules = Iterable(StarAdjacency, RowsAndCols, Regions)
}
