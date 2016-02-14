package org.grstcl.solvers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

abstract class PuzzleGrid[X, P <: Puzzle[X, P], G <: PuzzleGrid[X, P, G]](val puzzle: P)
  extends Map[(Int, Int), Option[X]]
{
  def inferences: Iterable[Inference[X, P, G]]
  def get(row: Int, col: Int): Option[Option[X]] = this.get(row -> col)

  def fromMap(map: Map[(Int, Int), Option[X]]): G

  def put(data: ((Int, Int), Option[X])): G =
  {
    if ((data._1._1 >= 0) &&
        (data._1._1 < puzzle.numRows) &&
        (data._1._2 >= 0) &&
        (data._1._2 < puzzle.numCols) &&
        this.get(data._1._1, data._1._2).isEmpty)
    {
      fromMap(this + data)
    }
    else
    {
      this.asInstanceOf[G]
    }
  }

  def isComplete: Boolean =
  {
    this.values.size == this.puzzle.numRows * this.puzzle.numCols
  }

  def isPartiallyValid: Boolean =
  {
    this.puzzle.rules.forall
    {
      rule => rule(this.asInstanceOf[puzzle.G])
    }
  }

  def isValid: Boolean =
  {
    this.isComplete && this.isPartiallyValid
  }

  def solutionPaths: Iterable[G] =
  {
    @tailrec
    def applyInferences(grid: G): G =
    {
      val inferred = grid.inferences.view.map {
        rule =>
          rule(grid).filter
          {
            loc =>
              (loc._1._1 >= 0) &&
              (loc._1._1 < puzzle.numRows) &&
                (loc._1._2 >= 0) &&
                (loc._1._2 < puzzle.numCols) &&
              grid.get(loc._1).isEmpty
          }
      }.find(_.nonEmpty)

      if (inferred.isDefined)
      {
        val newLocs = inferred.get

        applyInferences(newLocs.foldLeft(grid)
        {
          (grid, newLoc) => grid.put(newLoc)
        })
      }
      else
      {
        grid
      }
    }

    @tailrec
    def getSolutionPaths(queue: Queue[G], retVal: Iterable[G]): Iterable[G] =
    {
      if (queue.isEmpty)
      {
        retVal
      }
      else
      {
        val grid = queue.head

        if (grid.isPartiallyValid) {
          val firstOpenPos = (0 until puzzle.numRows).view.flatMap {
            row =>
              (0 until puzzle.numCols).view.map {
                col => (row, col)
              }
          }.find {
            case (r, c) => grid.get(r, c).isEmpty
          }

          if (firstOpenPos.isDefined) {
            val newAssumptions: Iterable[G] =
              puzzle.possibleValues.map { value => grid.put(firstOpenPos.get -> Some(value)) } ++
                {firstOpenPos.map { fop => grid.put(fop -> None)}: Iterable[G]}

            val newConclusions = newAssumptions.map
            {
              grid => applyInferences(grid)
            }

            getSolutionPaths(queue.drop(1) ++ newConclusions, retVal)
          }
          else {
            getSolutionPaths(queue.drop(1), retVal ++ Iterable(grid))
          }
        }
        else
        {
          getSolutionPaths(queue.drop(1), retVal)
        }
      }
    }

    getSolutionPaths(Queue(this.asInstanceOf[G]), Nil)
  }

  def getSolutions: Iterable[G] = {
    this.solutionPaths.filter(_.isValid)
  }
}
