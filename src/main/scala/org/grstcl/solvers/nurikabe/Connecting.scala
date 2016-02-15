package org.grstcl.solvers.nurikabe

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait Connecting
{
  def getConnectedThings(pos: (Int, Int), grid: NurikabeGrid): Set[(Int, Int)] =
  {
    val connectionType = grid.get(pos)

    if (connectionType.isEmpty)
    {
      Set.empty[(Int, Int)]
    }
    else
    {
      val test =
      {
        if (connectionType.get.isEmpty)
        {
          v: Option[Unit] => v.isEmpty
        }
        else
        {
          v: Option[Unit] => v.nonEmpty
        }
      }

      @tailrec
      def getConnectedSquares(testSquares: Queue[(Int, Int)], connectedSquares: Set[(Int, Int)]): Set[(Int, Int)] =
      {
        if (testSquares.isEmpty)
        {
          connectedSquares
        }
        else
        {
          val (testRow, testCol) = testSquares.head

          val newSquares = Iterable((testRow - 1, testCol),
            (testRow + 1, testCol),
            (testRow, testCol - 1),
            (testRow, testCol + 1)).filter
          {
            case (r, c) => !connectedSquares.contains((r, c)) &&
              grid.get(r, c).nonEmpty &&
              test(grid.get(r, c).get)
          }

          getConnectedSquares(testSquares.drop(1) ++ newSquares, connectedSquares ++ Set((testRow, testCol)) ++ newSquares)
        }
      }

      getConnectedSquares(Queue(pos), Set.empty[(Int, Int)])
    }
  }
}

trait AllConnections
{
  def getAllRegions(grid: NurikabeGrid): Iterable[Set[(Int, Int)]] =
  {
    @tailrec
    def getNeighbors(testQueue: Queue[(Int, Int)],
                     retVal: Map[(Int, Int), Int]):
      Map[(Int, Int), Int] =
    {
      if (testQueue.isEmpty)
      {
        retVal
      }
      else
      {
        val (row, col) = testQueue.head

        val region = retVal.getOrElse((row, col), retVal.values.fold(0){(a, b) => a.max(b)})
        val regionType = grid.get((row, col))

        val neighbors =
        if (regionType.isDefined)
        {
          Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
              .filter{neighbor => retVal.get(neighbor).isEmpty ||
                                  (retVal(neighbor) > region)}
        }
        else
        {
          Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
              .filter{neighbor => retVal.get(neighbor).isEmpty}
        }

        getNeighbors(testQueue ++ neighbors, retVal ++ Seq((row, col) -> region))
      }
    }

    val regionMap = getNeighbors(Queue((0, 0)), Map.empty[(Int, Int), Int])
    val regions = regionMap.groupBy(_._2).mapValues(_.keys)

    regions.values.map(_.toSet).filter(_.flatMap{spot => grid.get(spot)}.nonEmpty)
  }
}
