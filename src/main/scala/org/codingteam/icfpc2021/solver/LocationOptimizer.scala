package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.{Figure, Point, PointD, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.{Pi, abs, sqrt}
import scala.util.Random

class LocationOptimizer(problem: Problem) {
  val evaluator = new SolutionEvaluator(problem)

  def calcDiff(solution: Vector[Point], value: BigInt, v: Point): BigInt = {
    val translated = solution.map(p => p+v)
    val newValue = evaluator.evaluate(Solution(translated, null))
    newValue - value
  }

  def calcGradient(solution: Vector[PointD]): PointD = {
    val dx = Point(1, 0)
    val dy = Point(0, 1)
    val solutionInt = solution.map(_.round())
    val value = evaluator.evaluate(Solution(solutionInt, null))
    println(s"V: $value")

    var gradient = PointD(0,0)
    List(dx, dx*(-1), dy, dy*(-1)).foreach(dv => {
      val diff = calcDiff(solutionInt, value, dv)
      gradient = gradient + dv.toPointD() * diff.toDouble
    })
    gradient
  }

  def descent(solution: Vector[Point], step: Double, minGradient: Double, maxSteps: Int) : Vector[Point] = {
    var stop = false
    var currentSolution = solution.map(_.toPointD())
    var i = 0
    while (! stop) {
      val gradient = calcGradient(currentSolution)
      println(s"I: $i, G: $gradient")
      currentSolution = currentSolution.map(p => (p - gradient * step))
      i += 1
      stop = (i > maxSteps) || (gradient.abs() < minGradient)
    }
    currentSolution.map(_.round())
  }
}
