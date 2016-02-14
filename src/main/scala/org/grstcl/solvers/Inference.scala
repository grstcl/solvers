package org.grstcl.solvers

trait Inference[X, P <: Puzzle[X, P], G <: PuzzleGrid[X, P, G]]
  extends Function1[G, Iterable[((Int, Int), Option[X])]]
{
}
