package org.grstcl.solvers

trait Rule[X, P <: Puzzle[X, P], G <: PuzzleGrid[X, P, G]]
    extends Function1[G, Boolean]
{
}
