package org.codingteam.icfpc2021.force_solver

import org.codingteam.icfpc2021.{Edge, PointD, Problem, Solution}

import scala.collection.mutable

object ForceBasedSolver {
  def stepForward(problem: Problem, solution: Solution, steps: Int = 100): Solution = {
    val forces = mutable.Map[Int, PointD]().withDefaultValue(PointD(0, 0))
    val problemVertices = problem.figure.vertices.map(_.toPointD())
    var vertices = solution.vertices.map(_.toPointD())

    // Points outside the hole are trying to move inward
    for (i <- problem.figure.vertices.indices) {
      if (! problem.isPointInHole(solution.vertices(i))) {
        forces(i) += problem.holeCenter - vertices(i)
      }
    }

    // Stretched/compressed edges are trying to restore their lengths
    for (_ <- 0 until steps) {
      val edgeForces = mutable.Map[Int, PointD]().withDefaultValue(PointD(0, 0))
      for (Edge(v1, v2) <- problem.figure.edges) {
        val problemDist = (problemVertices(v2) - problemVertices(v1)).abs()
        val solV1 = vertices(v1)
        val solV2 = vertices(v2)
        val solutionDist = (solV2 - solV1).abs()
        val forceAbs = -(problemDist - solutionDist) / 2

        edgeForces(v1) += (solV2 - solV1).normalize() * forceAbs
        edgeForces(v2) += (solV1 - solV2).normalize() * forceAbs
      }

      val invN = 1.0 / (2* problem.figure.edges.length.toDouble)
      val avgForce = edgeForces.values.reduce(_+_) * invN

      for (i <- edgeForces.keys) {
        edgeForces(i) -= avgForce
      }

      for (i <- forces.keys) {
        forces(i) += edgeForces(i)
      }

      vertices = applyForces(vertices, forces)
    }

    solution.copy(vertices = vertices.map(_.round()))
  }

  private def applyForces(vertices: Seq[PointD], forces: mutable.Map[Int, PointD]): Vector[PointD] = {
    vertices.zipWithIndex.map { case (v, i) => v + (forces(i) / 100.0) }.toVector
  }
}
