package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Figure, Point, Problem, Solution}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

class PathSolver(problem: Problem) {
  private val holeDistances =
    problem.hole.sliding(2).map(pair => pair(0).distanceSq(pair(1))).toVector :+
      problem.hole(0).distanceSq(problem.hole.last)

  private val solutions = ArrayBuffer[
    (Int, List[(Boolean, Int)])
  ]()

  def solve(usedVerts: immutable.BitSet, maxSkips: Int, minLength: Int): Unit = {
    for (s <- holeDistances.indices) {
      println(s"${s} / ${holeDistances.length}")
      for (i <- problem.figure.vertices.indices) {
        search(s, List((false, i)), usedVerts, s, maxSkips, minLength)
      }
    }

    // FIXME: quadratic filterInPlace part (marked with `!` comment) could be faster
  
    println("Step 0: filter unrequired skips")
    solutions.filterInPlace { q => !q._2.head._1 }
    println("Step 1: remove short solutions")
    solutions.filterInPlace { q => !solutions.exists { w => q._2 == w._2.tail }} // !
    println("Step 2")
    solutions.mapInPlace { q => (q._1, q._2.reverse) }
    println("Step 3: remove short solutions")
    solutions.filterInPlace { q => !solutions.exists { w => q._2 == w._2.tail }} // !

    solutions.sortInPlaceBy { q => q._2.length }

    for (sol <- solutions) {
      print(s"${sol._1}:")
      for (v <- sol._2) {
        if (v._1)
          print(s"-\u001b[31m${v._2}\u001b[m")
        else
          print(s"_\u001b[32m${v._2}\u001b[m")
      }
      println()
    }
  }

  def search(
    startHoleVert: Int,
    figVerts: List[(Boolean, Int)],
    usedVerts: immutable.BitSet,
    holeVert: Int,
    skips: Int,
    minLength: Int,
  ): Unit = {
    if (usedVerts.contains(figVerts.head._2) || skips < 0)
      return
    val nextUsedVerts = usedVerts + figVerts.head._2

    if (figVerts.length >= minLength) {
      solutions.append((startHoleVert, figVerts))
    }

    if (skips != 0) {
      for (nextFigVert <- problem.figure.vertexNeighbours(figVerts.head._2)) {
        search(
          startHoleVert,
          (true, nextFigVert) :: figVerts,
          nextUsedVerts, holeVert, skips-1, minLength,
        )
      }
    }

    for (nextFigVert <- problem.figure.vertexNeighbours(figVerts.head._2)) {
      val range = problem.edgeDistRangeSqUnits(figVerts.head._2, nextFigVert)
      val holeDist = holeDistances(holeVert % holeDistances.length) * 1000000

      if (range._1 <= holeDist && holeDist <= range._2) {
        search(
          startHoleVert,
          (false, nextFigVert) :: figVerts,
          nextUsedVerts, holeVert+1, skips, minLength,
        )
      }
    }
  }
}
