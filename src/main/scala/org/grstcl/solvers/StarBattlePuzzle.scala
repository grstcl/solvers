package org.grstcl.solvers

case class StarBattlePuzzle(numStars: Int, numRows: Int, numCols: Int, regions: Iterable[Iterable[(Int, Int)]])
{
  def this(numStars: Int, dim: Int, regions: Iterable[Iterable[(Int, Int)]]) =
    this(numStars, dim, dim, regions)
}
