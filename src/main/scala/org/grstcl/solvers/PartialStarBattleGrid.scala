package org.grstcl.solvers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * Created by gsclark on 2/8/16.
  */
class PartialStarBattleGrid private(val puzzle: StarBattlePuzzle,
                                    private val values: Map[(Int, Int), Boolean])
  extends Iterable[((Int, Int), Boolean)] {
  def get(rc: (Int, Int)): Option[Boolean] = values.get(rc)

  override def iterator = values.iterator

  def this(puzzle: StarBattlePuzzle) = this(puzzle, Map.empty[(Int, Int), Boolean])

  def +(loc: ((Int, Int), Boolean)): PartialStarBattleGrid = {
    if ((loc._1._1 >= 0) &&
      (loc._1._1 < puzzle.numRows) &&
      (loc._1._2 >= 0) &&
      (loc._1._2 < puzzle.numCols) &&
      this.get(loc._1).isEmpty) {
      new PartialStarBattleGrid(puzzle, values + loc)
    }
    else {
      this
    }
  }

  def ++(iter: Iterable[((Int, Int), Boolean)]): PartialStarBattleGrid = {
    iter.foldLeft(this) {
      (grid, newItem) => grid + newItem
    }
  }

  def star(row: Int, col: Int): PartialStarBattleGrid = {
    ((this +((row, col), true)) ++ {
      ((row - 1) to (row + 1)).flatMap {
        r: Int =>
          ((col - 1) to (col + 1)).map {
            c: Int => ((r, c), false)
          }
      }
    }).rowsAndCols().regions()
  }

  def fillSection(section: List[(Int, Int)]): PartialStarBattleGrid = {
    val sectionVals = values
      .filter { case ((r, c), _) => section.contains((r, c)) }

    if (sectionVals.size < section.size) {
      val numStars = sectionVals.values.count(identity)
      val numBlanks = sectionVals.values.count { b => !b }

      if (numStars == puzzle.numStars) {
        this ++ section.map { indices => indices -> false }
      }
      else if (numBlanks == puzzle.numCols - puzzle.numStars) {
        section.filter
        {
          indices => this.get(indices).isEmpty
        }
            .foldLeft(this)
        {
          case (grid, (r, c)) => grid.star(r, c)
        }
      }
      else {
        this
      }
    }
    else {
      this
    }
  }

  def rowsAndCols(): PartialStarBattleGrid =
  {
    val rowedGrid = (0 until puzzle.numRows).foldLeft(this)
    {
      (grid, rowNum) => grid.fillSection((0 until puzzle.numCols).map{c => (rowNum, c)}.toList)
    }

    (0 until puzzle.numCols).foldLeft(this)
    {
      (grid, colNum) => grid.fillSection((0 until puzzle.numRows).map{r => (r, colNum)}.toList)
    }
  }

  def regions(): PartialStarBattleGrid =
  {
    puzzle.regions.foldLeft(this)
    {
      (grid, region) => grid.fillSection(region.toList)
    }
  }

  def guesses(): Iterable[PartialStarBattleGrid] =
  {
    val firstSpot = (0 until puzzle.numRows).view.flatMap
    {
      r => (0 until puzzle.numCols).view.map
        {
          c => (r, c)
        }
    }.find
    {
      case (r, c) => values.get((r, c)).isEmpty
    }

    firstSpot.toList.flatMap
    {
      case (r, c) =>
        List(this.star(r, c).rowsAndCols().regions(),
             (this + ((r, c), false)).rowsAndCols().regions())
    }
  }

  def solve(): Iterable[PartialStarBattleGrid] =
  {
    @tailrec
    def nextGuesses(gridQueue: Queue[PartialStarBattleGrid],
                    retVal: Iterable[PartialStarBattleGrid] = Iterable.empty[PartialStarBattleGrid]):
      Iterable[PartialStarBattleGrid] = {
      if (gridQueue.isEmpty) {
        retVal
      }
      else {
        val grid = gridQueue.head

        if (grid.isPartiallyValid) {
          if (grid.guesses().isEmpty) {
            if (grid.isValid) {
              nextGuesses(gridQueue.drop(1), retVal ++ Iterable(grid))
            }
            else {
              nextGuesses(gridQueue.drop(1), retVal)
            }
          }
          else {
            nextGuesses(gridQueue.drop(1) ++ grid.guesses(), retVal)
          }
        }
        else
        {
          nextGuesses(gridQueue.drop(1), retVal)
        }
      }
    }

    nextGuesses(Queue(this))
  }

  def isComplete: Boolean =
  {
    (0 until puzzle.numRows).flatMap
    {
      r => (0 until puzzle.numCols).map
        {
          c => (r, c)
        }
    }
      .forall
    {
      case (r, c) => this.get(r, c).nonEmpty
    }
  }

  def isPartiallyValid: Boolean =
  {
    lazy val adjacency =
    {
      val stars = this.values.filter{case ((r, c), t) => t}.keys

      stars.forall
      {
        case (r, c) =>
          ((r - 1) to (r + 1)).flatMap
          {
            row =>
              ((c - 1) to (c + 1)).map
              {
                col => (row, col)
              }
          }
            .flatMap{case (row, col) => this.get(row, col)}
            .count(identity) == 1
      }
    }

    lazy val rows =
    {
      (0 until puzzle.numRows).forall
      {
        row =>
          val rowVals = (0 until puzzle.numCols).map
          {
            col => (row, col)
          }
            .flatMap{case (rowIndex, colIndex) => this.get(rowIndex, colIndex)}

          (rowVals.count(identity) <= puzzle.numStars) &&
            (rowVals.count{v => !v} <= puzzle.numCols - puzzle.numStars)
      }
    }

    lazy val cols =
    {
      (0 until puzzle.numCols).forall
      {
        col =>
          val colVals = (0 until puzzle.numRows).map
          {
            row => (row, col)
          }
            .flatMap{case (rowIndex, colIndex) => this.get(rowIndex, colIndex)}

          (colVals.count(identity) <= puzzle.numStars) &&
            (colVals.count{v => !v} <= puzzle.numRows - puzzle.numStars)
      }
    }

    lazy val regions = puzzle.regions.forall
    {
      region =>
        val regionVals = region.flatMap{case (row, col) => this.get(row, col)}
        (regionVals.count(identity) <= puzzle.numStars) &&
          (regionVals.count{v => !v} <= region.size - puzzle.numStars)
    }

    adjacency && rows && cols && regions
  }

    def isValid: Boolean =
    {
      this.isComplete && this.isPartiallyValid
    }

    def printGrid(): Unit =
  {
    (0 until puzzle.numRows).foreach
    {
      r =>
        (0 until puzzle.numCols).foreach
        {
          c =>
            val value = values.get((r, c))

            if (value.isEmpty)
            {
              print("? ")
            }
            else if (value.get)
            {
              print("* ")
            }
            else
            {
              print("_ ")
            }
        }

        println()
    }
    println()
  }
}