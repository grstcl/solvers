package org.grstcl.solvers

trait PuzzleGrid[X, T <: Puzzle[X, T]]
{
  protected val values: Map[(Int, Int), Option[X]]

  def get(row: Int, col: Int): Option[Option[X]] = this.values.get(row -> col)

  val puzzle: T

  def isComplete: Boolean =
  {
    this.values.flatten.size == this.puzzle.numRows * this.puzzle.numCols
  }

  def isPartiallyValid: Boolean =
  {
    this.puzzle.rules.forall
    {
      rule => rule(this)
    }
  }

  def isValid: Boolean =
  {
    this.isComplete && this.isPartiallyValid
  }
}
