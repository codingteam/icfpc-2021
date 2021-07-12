package org.codingteam.icfpc2021.force_solver

import org.codingteam.icfpc2021.validator.{EdgeCheckResult, SolutionValidator}
import org.codingteam.icfpc2021.{Edge, Point, PointD, Problem, Solution}

import java.awt.Polygon
import java.security.PrivilegedExceptionAction
import scala.collection.mutable
import scala.util.Random

object ForceBasedSolver {
  val random = new Random()

  def projectionToSegment(p: PointD, p1: PointD, p2: PointD): Option[PointD] = {
    val dp = p2 - p1
    val dir = dp.normalized()
    val dp1 = (p - p1)
    val projectionLength = dir dot dp1
    val segmentLength = (p2 - p1).abs()
    if (projectionLength <= 0 || projectionLength >= segmentLength) {
      None
    } else {
      Some(p1 + dir * projectionLength)
    }
  }

  def nearestPointOfHole(problem: Problem, p: PointD) : (PointD, Double) = {
    var distance = Double.MaxValue
    var nearest = problem.hole(0).toPointD()
    problem.holeEdges.foreach(e => {
      val h1 = problem.hole(e.vertex1).toPointD()
      val h2 = problem.hole(e.vertex2).toPointD()
      projectionToSegment(p, h1, h2) match {
        case None => {}
        case Some(proj) => {
          val d = (proj - p).abs()
          if (d < distance) {
            distance = d
            nearest = proj
          }
        }
      }
    }
    )
    (nearest, distance)
  }

  def groupToSet(group: Vector[(Int,Int,Int)]) : mutable.Set[Int] = {
    val result = mutable.TreeSet[Int]()
    for ((i,j,k) <- group) {
      result += i
      result += j
      result += k
    }
    result
  }

  def edgesOfGroup(group: Seq[Int], edges: Seq[Edge]): Seq[Edge] = {
    for {
      edge <- edges
      if (group.contains(edge.vertex1) && group.contains(edge.vertex2))
    } yield edge
  }

  def stepForward(problem: Problem, solution: Solution, steps: Int = 100): Solution = {
    val forces = mutable.Map[Int, PointD]().withDefaultValue(PointD(0, 0))
    val problemVertices = problem.figure.vertices.map(_.toPointD())
    var vertices = solution.vertices.map(_.toPointD())

    val validator = new SolutionValidator(problem)

    // Stretched/compressed edges are trying to restore their lengths
    for (_ <- 0 until steps) {
      // Points outside the hole are trying to move inward
      for ((v, i) <- vertices.view.zipWithIndex) {
        if (!validator.isPointInHole(v)) {
          val nearest = nearestPointOfHole(problem, v)._1
          //println(s"V.$i: nearest = $nearest")
          forces(i) = (nearest - v) // / 10.0
        }
      }

      val edgeForces = mutable.Map[Int, PointD]().withDefaultValue(PointD(0, 0))
      for (Edge(v1, v2) <- problem.figure.edges) {
        val problemDist = (problemVertices(v2) - problemVertices(v1)).abs()
        val solV1 = vertices(v1)
        val solV2 = vertices(v2)
        val solutionDist = (solV2 - solV1).abs()
        val forceAbs = -(problemDist - solutionDist) / 2

        if (solutionDist != 0.0) {
          edgeForces(v1) += (solV2 - solV1).normalize() * forceAbs
          edgeForces(v2) += (solV1 - solV2).normalize() * forceAbs
        } else {
          val heading = PointD(random.between(-1, 2), random.between(-1, 2))
          edgeForces(v1) += heading.normalize() * forceAbs
          edgeForces(v1) -= heading.normalize() * forceAbs
        }
        forces(v1) += edgeForces(v1)
        forces(v2) += edgeForces(v2)
      }

      val invN = 1.0 / (2* problem.figure.edges.length.toDouble)
      val avgForce = edgeForces.values.reduce(_+_) * invN

      //for (i <- forces.keys) {
      //  println(s"V.$i: F = ${forces(i)}")
      //}

      //for (i <- forces.keys) {
      //  forces(i) -= avgForce
      //}

      for (group <- problem.figure.triangleGroups) {
        val groupVerts = groupToSet(group)
        val groupEdges = edgesOfGroup(groupVerts.toSeq, problem.figure.edges)
        val allEdgesGood = groupEdges.forall(e => validator.checkEdgeLength(solution, e) == EdgeCheckResult.Exact)
        if (allEdgesGood) {
          val nForces = groupVerts.size.toDouble
          val forceSum = groupVerts.map(i => forces(i)).reduce(_+_)
          for (i <- groupVerts) {
            forces(i) = forceSum / nForces
          }
        }
      }

      vertices = applyForces(vertices, forces)
    }

    solution.copy(vertices = vertices.map(_.round()))
  }

  private def applyForces(vertices: Seq[PointD], forces: mutable.Map[Int, PointD]): Vector[PointD] = {
    vertices.zipWithIndex.map { case (v, i) => v + (forces(i) / 100.0) }.toVector
  }
}
