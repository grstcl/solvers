package org.grstcl.solvers.nurikabe

import org.grstcl.solvers.{Inference, PuzzleGrid}

object No2x2BlackInference extends Inference[Unit, NurikabePuzzle, NurikabeGrid]
{
  override def apply(grid: NurikabeGrid): Iterable[((Int, Int), Option[Unit])] =
  {
    val squares = (0 until (grid.puzzle.numRows - 1)).view.flatMap
    {
      row =>
        (0 until (grid.puzzle.numCols - 1)).view.map
        {
          col => Seq((row, col), (row + 1, col), (row, col + 1), (row + 1, col + 1))
        }
    }
      .map
    {
      square => square.map{pos => (pos, grid.get(pos))}
    }

    val spot = squares.find
    {
      square => square.count{case (pos, value) => value.nonEmpty && value.get.isEmpty} == 3
    }

    spot.flatMap
    {
      square => square.find{case (pos, value) => value.isEmpty}.map(_._1 -> {Some(Unit): Option[Unit]})
    }.toIterable
  }
}

object BlackConnectedInference extends Inference[Unit, NurikabePuzzle, NurikabeGrid] with AllConnections
{
  override def apply(grid: NurikabeGrid): Iterable[((Int, Int), Option[Unit])] =
  {
    val blackRegions = getAllRegions(grid).filter {
      region: Set[(Int, Int)] =>
        region.exists{ pos: (Int, Int) => grid.get(pos).isDefined && grid(pos).isEmpty }
    }

    if (blackRegions.size > 1)
    {
      blackRegions.flatMap
      {
        region =>
          val exits = region.map{pos => (pos, grid.get(pos))}.filter(_._2.isEmpty)
          if (exits.size == 1)
          {
            exits.headOption.map(_._1)
          }
          else
          {
            None
          }
      }.map{pos => (pos, None)}
    }
    else
    {
      Iterable.empty
    }
  }
}

class NurikabeGrid extends PuzzleGrid[Unit, NurikabePuzzle, NurikabeGrid]
{

}
