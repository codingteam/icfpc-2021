package org.codingteam.icfpc2021.force_solver

import org.codingteam.icfpc2021.{Edge, PointD, Problem, Solution}

import java.awt.Polygon
import scala.collection.mutable
import scala.util.Random

object ForceBasedSolver {
  val random = new Random()
  def stepForward(problem: Problem, solution: Solution, steps: Int = 100): Solution = {
    val forces = mutable.Map[Int, PointD]().withDefaultValue(PointD(0, 0))
    val problemVertices = problem.figure.vertices.map(_.toPointD())
    var vertices = solution.vertices.map(_.toPointD())

    val hole = new Polygon(problem.hole.map(_.x.intValue).toArray, problem.hole.map(_.y.intValue).toArray, problem.hole.size)

    // Stretched/compressed edges are trying to restore their lengths
    for (_ <- 0 until steps) {
      // Points outside the hole are trying to move inward
      for ((v, i) <- vertices.view.zipWithIndex) {
        if (!hole.contains(v.x, v.y)) {
          forces(i) = (problem.holeCenter - v) / 10.0
        }
      }

      for (Edge(v1, v2) <- problem.figure.edges) {
        val problemDist = (problemVertices(v2) - problemVertices(v1)).abs()
        val solV1 = vertices(v1)
        val solV2 = vertices(v2)
        val solutionDist = (solV2 - solV1).abs()
        val forceAbs = -(problemDist - solutionDist) / 2

        if (solutionDist != 0.0) {
          forces(v1) += (solV2 - solV1).normalize() * forceAbs
          forces(v2) += (solV1 - solV2).normalize() * forceAbs
        } else {
          val heading = PointD(random.between(-1, 2), random.between(-1, 2))
          forces(v1) += heading.normalize() * forceAbs
          forces(v1) -= heading.normalize() * forceAbs
        }
      }

      val invN = 1.0 / (2* problem.figure.edges.length.toDouble)
      val avgForce = forces.values.reduce(_+_) * invN

      for (i <- forces.keys) {
        forces(i) -= avgForce
      }

      vertices = applyForces(vertices, forces)
    }

    solution.copy(vertices = vertices.map(_.round()))
  }

  private def applyForces(vertices: Seq[PointD], forces: mutable.Map[Int, PointD]): Vector[PointD] = {
    vertices.zipWithIndex.map { case (v, i) => v + (forces(i) / 100.0) }.toVector
  }
}
