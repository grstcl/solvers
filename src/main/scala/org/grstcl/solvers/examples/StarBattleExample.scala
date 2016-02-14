package org.grstcl.solvers.examples

import org.grstcl.solvers.starbattle.{StarBattleGrid, StarBattlePuzzle}

object StarBattleExample
{
  def main(args: Array[String]): Unit =
  {
    val puzzle = new StarBattlePuzzle(1, 5, (0 until 5).map{r => (0 until 5).map{c => (r, c)}})
    val grid = new StarBattleGrid(puzzle)
    grid.getSolutions.foreach(println)
  }
}
