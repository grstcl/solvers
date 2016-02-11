package org.grstcl.solvers

import org.scalatest._

class StarBattleTest extends FlatSpec with Matchers {
  "A basic Star Battle grid" should "have a solution path" in
  {
    val puzzle = new StarBattlePuzzle(1, 5, (0 until 5).map{r => (0 until 5).map{c => (r, c)}})
    new PartialStarBattleGrid(puzzle).solve().foreach(_.printGrid())
  }
}