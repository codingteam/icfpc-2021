package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.{Figure, Point, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.{Pi, abs, sqrt}

case class Options(useRotations : Boolean, useTranslations: Boolean)

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
case class MirrorOne(vertexIndex: Int) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    val figure = Figure(problem.figure.edges, solution)
    DumbSolver.foldInOne(figure, vertexIndex) match {
      case None => solution
      case Some(newFigure) => newFigure.vertices
    }
  }
}

case class Rotate(angle : Double) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    RotationSolver.rotate_by(angle, solution)
  }
}

case class Move(vector: Point) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    solution.map(p => p + vector)
  }
}

case class MirrorX() extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    DumbSolver.mirrorX(solution)
  }
}

case class MirrorY() extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    DumbSolver.mirrorY(solution)
  }
}

case class TransposeXY() extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    DumbSolver.transposeXY(solution)
  }
}

class SolutionOptimizer(problem: Problem) {
  val evaluator = new SolutionEvaluator(problem)
  val validator = new SolutionValidator(problem)
  def possibleActions(solution: Vector[Point], options: Options): Seq[Action] = {
    val n = solution.length
    val figure = Figure(problem.figure.edges, solution)
    val idxs = 0 to (n-1)
    val wobbles = idxs.map(i => Wobble(i, delta=5)) : Seq[Action]
    val mirrors = idxs.flatMap(i => {
        val neighbours = figure.vertexNeighbours(i)
        if (neighbours.length == 2) {
          Some(MirrorOne(i))
        } else {
          None
        }
      }
    ) : Seq[Action]

    val rotations = {
      if (options.useRotations) {
        (0 until 360).map(i => {
          val angle = 2 * Pi / 360.0
          Rotate(angle)
        })
      } else {
        List()
      }
    }

    val sz = problem.holeRect.size
    val (maxDx, maxDy) = (sz.x / 2, sz.y / 2)
    lazy val moves = {
      if (options.useTranslations) {
        for {dx <- -maxDx to maxDy
             dy <- -maxDy to maxDy}
        yield Move(Point(dx, dy))
      } else {
        List()
      }
    }

    wobbles ++ mirrors ++ rotations ++ moves ++ List(TransposeXY(), MirrorX(), MirrorY())
  }

  def optimizeOnce(solution: Vector[Point], options: Options): Vector[Point] = {
    val actions = possibleActions(solution, options)
    //println(s"A: $actions")
    val sols = actions.map(a => Solution(a.apply(problem, solution), null))
    val validSols = sols.filter(sol => validator.validate(sol))
    val results = validSols.map(sol => evaluator.evaluate(sol))
    //println(s"R: $results")
    val bestIdx = results.zipWithIndex.minBy(_._1)._2
    validSols(bestIdx).vertices
  }
}
