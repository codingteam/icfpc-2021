package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Figure, Point, Problem, Solution}

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PathSolver(problem: Problem) {
  private val holeDistances =
    problem.hole.sliding(2).map(pair => pair(0).distanceSq(pair(1))).toVector :+
      problem.hole(0).distanceSq(problem.hole.last)

  private val solutions = ArrayBuffer[
    (Int, List[(Option[Int], Int)])
  ]()

  var maxLen = 0

  def solve(
    usedVerts: immutable.BitSet, maxSkips: Int, minLength: Int,
    requiredHoles: immutable.BitSet,
  ): Unit = {
    for (s <- requiredHoles) {
      println(s"${s} / ${holeDistances.length}; solutions=${solutions.length}; max=${maxLen}")
      for (i <- problem.figure.vertices.indices) {
        figVerts2(i) = Some(s)
        search(
          List((Some(s), i)),
          usedVerts + i,
          immutable.BitSet(holeDistances.indices.toSeq: _*) - s,
          maxSkips,
          minLength,
        )
        figVerts2.remove(i)
      }
    }

    // FIXME: quadratic filterInPlace part (marked with `!` comment) could be faster

    // println("Step -1: filter by required holes")
    // solutions.filterInPlace { q =>
    //   val w = q._2.filter({case (a, b) => a != None}).map({case (a, _) => a.get}).toSet
    //   requiredHoles.subsetOf(w)
    // }
  
    println(s"Total solutions: ${solutions.length}, max len = ${maxLen}")
    if (solutions.length > 10000) {
      println("Too many solutions!")
      return
    }

    println("Step 0: filter unrequired skips")
    solutions.filterInPlace { q => q._2.head._1 != None }
    println("Step 1: remove short solutions")
    solutions.filterInPlace { q => !solutions.exists { w => q._2 == w._2.tail }} // !
    println("Step 2")
    solutions.mapInPlace { q => (q._1, q._2.reverse) }
    println("Step 3: remove short solutions")
    solutions.filterInPlace { q => !solutions.exists { w => q._2 == w._2.tail }} // !

    solutions.sortInPlaceBy { q => q._2.length }

    for (sol <- solutions) {
      for (v <- sol._2) {
        v._1 match {
          case None => print(s"_\u001b[31m${v._2}\u001b[m")
          case Some(x) => print(s"_\u001b[32m${x},${v._2}\u001b[m")
        }
      }
      println()
    }
  }

  val figVerts2 = new mutable.HashMap[Int, Option[Int]]

  def search(
    figVerts: List[(Option[Int], Int)],
    usedVerts: immutable.BitSet,
    availableHoleVerts: immutable.BitSet,
    skips: Int,
    minLength: Int,
  ): Unit = {
    maxLen = math.max(maxLen, figVerts.length)
    if (figVerts.length >= minLength) {
      solutions.append((0, figVerts))
    }

    if (skips != 0) {
      for (nextFigVert <- problem.figure.vertexNeighbours(figVerts.head._2)) {
        if (!usedVerts.contains(nextFigVert)) {
          figVerts2(nextFigVert) = None
          search(
            (None, nextFigVert) :: figVerts,
            usedVerts + nextFigVert,
            availableHoleVerts, skips-1, minLength,
          )
          figVerts2.remove(nextFigVert)
        }
      }
    }

    var prev: Int = figVerts.find({ case (a, b) => a != None }).get._1.get

    for ((nextFigVert, nextHole) <- vertexNeighbours((figVerts.head._2, prev)) ) {
      if (availableHoleVerts.contains(nextHole)
          && !usedVerts.contains(nextFigVert)
          && validate(nextFigVert, nextHole)
      ) {
        figVerts2(nextFigVert) = Some(nextHole)
        search(
          (Some(nextHole), nextFigVert) :: figVerts,
          usedVerts + nextFigVert,
          availableHoleVerts - nextHole, skips, minLength,
        )
        figVerts2.remove(nextFigVert)
      }
    }
  }

  private def validate(figA: Int, holeA: Int): Boolean = {
    for (figB <- problem.figure.vertexNeighbours(figA)) {
      val (figDistMin, figDistMax) = problem.edgeDistRangeSqUnits(figA, figB)

      figVerts2.getOrElse(figB, None) foreach { holeB =>
        val holeDist = problem.hole(holeA).distanceSq(problem.hole(holeB)) * 1000000
        if (!(figDistMin <= holeDist && holeDist <= figDistMax))
          return false
      }
    }
    return true
  }

  // (vert, hole) => (vert, hole)
  val vertexNeighbours = {
    val result = new mutable.HashMap[(Int, Int), Vector[(Int, Int)]].withDefault { key => Vector() }

    for (f1 <- problem.figure.vertices.indices) {
      for (f2 <- problem.figure.vertexNeighbours(f1)) {
        val (figDistMin, figDistMax) = problem.edgeDistRangeSqUnits(f1, f2)

        for ((h1v, h1) <- problem.hole.zipWithIndex) {
          for ((h2v, h2) <- problem.hole.zipWithIndex) {
            val holeDist = h1v.distanceSq(h2v) * 1000000
            if (figDistMin <= holeDist && holeDist <= figDistMax && h1 != h2 &&
              !problem.segmentGoesOutsideTheHole(h1v, h2v)
              && (h1, h2) != (57, 12) && (h1, h2) != (12, 57) // WTF
              )
              result((f1, h1)) :+= ((f2, h2))
          }
        }
      }
    }

    result
  }

}
