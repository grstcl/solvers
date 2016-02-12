package org.grstcl.solvers

trait Puzzle[X, T <: Puzzle[X, T]]
{
  val numRows: Int
  val numCols: Int
  def rules: Iterable[_ <: Rule[X, T]]
}
