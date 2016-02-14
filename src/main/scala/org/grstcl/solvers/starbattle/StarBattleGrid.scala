package org.grstcl.solvers.starbattle

import scala.{Boolean => ActualBoolean}

import org.grstcl.solvers.{Inference, PuzzleGrid}

import StarBattleGrid._

object StarBattleGrid
{
  type Boolean = Option[Unit]

  implicit def forwardConversion(b: ActualBoolean): Boolean =
  {
    b match
    {
      case true => Some(Unit)
      case false => None
    }
  }

  implicit def backwardConversion(b: Boolean): ActualBoolean =
  {
    b.nonEmpty
  }
}

object NoAdjacentStars extends Inference[Unit, StarBattlePuzzle, StarBattleGrid]
{
  override def apply(grid: StarBattleGrid): Iterable[((Int, Int), Boolean)] =
  {
    grid.filter
    {
      case (_, v: Option[Unit]) => v.nonEmpty
    }
    .keys
    .flatMap
    {
      case (r, c) =>
        ((r - 1) to (r + 1)).flatMap
        {
          row => ((c - 1) to (c + 1)).map
          {
            col => ((row, col), None)
          }
        }
    }
  }
}

trait SectionFiller {
  protected def fillSection(section: List[(Int, Int)],
                            grid: StarBattleGrid):
    Iterable[((Int, Int), Option[Unit])] =
  {
    val sectionVals: List[((Int, Int), Boolean)] =
      section.flatMap[((Int, Int), Boolean), List[((Int, Int), Boolean)]]
        {
          pos =>
            grid.get(pos).map{v => (pos, v)}
        }

    if (sectionVals.size < section.size) {
      val numStars = sectionVals.count(_._2.isDefined)
      val numBlanks = sectionVals.count(_._2.isEmpty)

      if (numStars == grid.puzzle.numStars) {
        section.map { indices => indices -> None }
      }
      else if (numBlanks == section.size - grid.puzzle.numStars) {
        section.map{indices => indices -> {Some(Unit): Option[Unit]}}
      }
      else {
        Nil
      }
    }
    else {
      Nil
    }
  }
}

object RowsAndColsInference extends Inference[Unit, StarBattlePuzzle, StarBattleGrid] with SectionFiller
{
  override def apply(grid: StarBattleGrid): Iterable[((Int, Int), Option[Unit])] =
  {
    val rows = (0 until grid.puzzle.numRows).flatMap
    {
      rowIndex =>
        val row = (0 until grid.puzzle.numCols).map{colIndex => (rowIndex, colIndex)}
        fillSection(row.toList, grid)
    }

    val cols = (0 until grid.puzzle.numCols).flatMap
    {
      colIndex =>
        val col = (0 until grid.puzzle.numRows).map{rowIndex => (rowIndex, colIndex)}
        fillSection(col.toList, grid)
    }

    (rows ++ cols).distinct
  }
}

object RegionsInference extends Inference[Unit, StarBattlePuzzle, StarBattleGrid] with SectionFiller
{
  override def apply(grid: StarBattleGrid): Iterable[((Int, Int), Option[Unit])] =
  {
    grid.puzzle.regions.flatMap
    {
      region => fillSection(region.toList, grid)
    }
  }
}

/**
  * Created by gsclark on 2/8/16.
  */
class StarBattleGrid private(puzzle: StarBattlePuzzle,
                             protected val entries: Map[(Int, Int), Boolean])
  extends PuzzleGrid[Unit, StarBattlePuzzle, StarBattleGrid](puzzle)
{
  def this(puzzle: StarBattlePuzzle) = this(puzzle, Map.empty[(Int, Int), Boolean])

  def get(rc: (Int, Int)): Option[Boolean] = entries.get(rc)

  override def iterator = entries.iterator

  override def +[B >: Boolean](v: ((Int, Int), B)) = entries + v
  override def -(key: (Int, Int)) = new StarBattleGrid(puzzle, entries - key)

  override def fromMap(map: Map[(Int, Int), Boolean]): StarBattleGrid =
    new StarBattleGrid(puzzle, map)

  def ++(iter: Iterable[((Int, Int), Boolean)]): StarBattleGrid = {
    iter.foldLeft(this) {
      (grid, newItem) => grid put newItem
    }
  }

  override def inferences: Iterable[Inference[Unit, StarBattlePuzzle, StarBattleGrid]] =
    Iterable(NoAdjacentStars, RowsAndColsInference, RegionsInference)

  override def put(data: ((Int, Int), Option[Unit])): StarBattleGrid = {
    val pre = super.put(data)

    if (data._2.nonEmpty) {
      val row = data._1._1
      val col = data._1._2

      ((row - 1) to (row + 1)).flatMap {
        r =>
          ((col - 1) to (col + 1)).map {
            c => ((r, c), None)
          }
      }.foldLeft(pre) {
        case (grid, emptySpot) => grid.put(emptySpot)
      }
    }
    else
    {
      pre
    }
  }

  override def toString(): String =
  {
    (0 until puzzle.numRows).map
    {
      r =>
        (0 until puzzle.numCols).map
        {
          c =>
            val value = entries.get((r, c))

            if (value.isEmpty)
            {
              "?"
            }
            else if (value.get)
            {
              "*"
            }
            else
            {
              "_"
            }
        }.mkString(" ")
    }.mkString("\n") + "\n"
  }
}