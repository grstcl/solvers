package org.grstcl.solvers

trait Puzzle[X, P <: Puzzle[X, P]]
{
  type G <: PuzzleGrid[X, P, G]

  val numRows: Int
  val numCols: Int
  def rules: Iterable[Rule[X, P, G]]
  def possibleValues: Iterable[X]
}
