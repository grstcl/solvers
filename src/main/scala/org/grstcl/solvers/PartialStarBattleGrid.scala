package org.grstcl.solvers

/**
  * Created by gsclark on 2/8/16.
  */
class PartialStarBattleGrid private(puzzle: StarBattlePuzzle,
                                    private val values: Map[(Int, Int), Boolean])
  extends Iterable[((Int, Int), Boolean)]
{
  def get(rc: (Int, Int)): Option[Boolean] = values.get(rc)
  override def iterator = values.iterator

  def this(numRows: Int, numCols: Int) = this(numRows, numCols, Map.empty[(Int, Int), Boolean])

  def +(loc: ((Int, Int), Boolean)): PartialStarBattleGrid =
  {
    if ((loc._1._1 >= 0) &&
        (loc._1._1 < puzzle.numRows) &&
        (loc._1._2 >= 0) &&
        (loc._1._2 < puzzle.numCols))
    {
      new PartialStarBattleGrid(puzzle, values + loc)
    }
    else
    {
      this
    }
  }

  def ++(iter: Iterable[((Int, Int), Boolean)]): PartialStarBattleGrid =
  {
    iter.foldLeft(this)
    {
      (grid, newItem) => grid + newItem
    }
  }

  def star(row: Int, col: Int): PartialStarBattleGrid =
  {
    (this + ((row, col), true)) ++
      {{((row - 1) to (row + 1)).flatMap
      {
        r: Int =>
          ((col - 1) + (col + 1)).map
          {
            c: Int => ((r, c), false)
          }
      }}: Iterable[((Int, Int), Boolean)]}
  }
}
