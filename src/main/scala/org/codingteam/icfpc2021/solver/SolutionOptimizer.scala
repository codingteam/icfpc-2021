package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.{Figure, Point, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.{abs, sqrt}

sealed abstract class Action() {
  def apply(problem: Problem, solution: Vector[Point]) : Vector[Point]
}
case class Wobble(vertexIndex: Int, delta: Int) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    val res = DumbSolver.wobbleOne(problem, solution, vertexIndex, delta)
    if (res.length >= 1) {
      res(0)
    } else {
      solution
    }
  }
}
case class Mirror(vertexIndex: Int) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    val figure = Figure(problem.figure.edges, solution)
    DumbSolver.foldInOne(figure, vertexIndex) match {
      case None => solution
      case Some(newFigure) => newFigure.vertices
    }
  }
}

class SolutionOptimizer(problem: Problem) {
  val evaluator = new SolutionEvaluator(problem)
  val validator = new SolutionValidator(problem)
  def possibleActions(solution: Vector[Point]): Seq[Action] = {
    val n = solution.length
    val figure = Figure(problem.figure.edges, solution)
    val idxs = 0 to (n-1)
    val wobbles = idxs.map(i => Wobble(i, delta=5)) : Seq[Action]
    val mirrors = idxs.flatMap(i => {
        val neighbours = figure.vertexNeighbours(i)
        if (neighbours.length == 2) {
          Some(Mirror(i))
        } else {
          None
        }
      }
    ) : Seq[Action]
    wobbles ++ mirrors
  }

  def optimizeOnce(solution: Vector[Point]): Vector[Point] = {
    val actions = possibleActions(solution)
    //println(s"A: $actions")
    val sols = actions.map(a => Solution(a.apply(problem, solution), null))
    val validSols = sols.filter(sol => validator.validate(sol))
    val results = validSols.map(sol => evaluator.evaluate(sol))
    //println(s"R: $results")
    val bestIdx = results.zipWithIndex.minBy(_._1)._2
    validSols(bestIdx).vertices
  }
}
