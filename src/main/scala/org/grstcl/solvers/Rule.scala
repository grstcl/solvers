package org.grstcl.solvers

trait Rule[X, T <: Puzzle[X, T]]
    extends Function1[PuzzleGrid[X, T], Boolean]
{
}
