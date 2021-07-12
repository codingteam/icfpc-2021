package org.codingteam.icfpc2021.solver

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.{Edge, Figure, Point, Problem, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.{Pi, abs, sqrt}
import scala.util.Random

case class Options(nIterations: Int, useWobbles: Boolean, correctByWobbles: Boolean, useRotations : Boolean, useTranslations: Boolean, useFolds: Boolean, wobbleDelta: Int = 5, translationDelta: Int = 10)

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

case class FoldAroundEdge(edge: Edge, dir: Boolean) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    DumbSolver.unfoldAroundEdge(problem, solution, edge, dir)
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

case class MoveSingle(vertexIndex: Int, newVertex: Point) extends Action {
  override def apply(problem: Problem, solution: Vector[Point]) : Vector[Point] = {
    solution.updated(vertexIndex, newVertex)
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
  val graph = new GraphSolver(problem)

  def singleRotations(solution: Vector[Point], vertexIndex: Int): Seq[Action] = {
    val p = solution(vertexIndex).toPointD()
    val baseVertIdx = problem.figure.vertexNeighbours(vertexIndex)(0)
    val base = solution(baseVertIdx)
    val epsilon = problem.epsilon.toDouble
    val radius = (p - base.toPointD()).abs()
    DumbSolver.brezenhem(epsilon, radius).map(dv => MoveSingle(vertexIndex, base+dv))
  }

  def allSingleRotations(solution: Vector[Point]): Seq[Action] = {
    solution.zipWithIndex.flatMap { case (p, i) =>
      if (problem.figure.vertexNeighbours(i).length == 1) {
        singleRotations(solution, i)
      } else {
        List()
      }
    }
  }

  def possibleActions(solution: Vector[Point], options: Options): Seq[Action] = {
    val n = solution.length
    val figure = Figure(problem.figure.edges, solution)
    val idxs = 0 to (n-1)
    val wobbles =
      if (options.useWobbles) {
        idxs.map(i => Wobble(i, delta=options.wobbleDelta)) : Seq[Action]
      } else {
        List()
      }
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
          val angle = i* 2 * Pi / 360.0
          Rotate(angle)
        })
      } else {
        List()
      }
    }

    //val sz = problem.holeRect.size
    val (maxDx, maxDy) = (options.translationDelta, options.translationDelta)
    lazy val moves = {
      if (options.useTranslations) {
        for {dx <- -maxDx to maxDy
             dy <- -maxDy to maxDy}
        yield Move(Point(dx, dy))
      } else {
        List()
      }
    }

    val folds =
      if (options.useFolds) {
        graph.edgesGoodForFolds.flatMap(edge =>
            List(FoldAroundEdge(edge, true),
              FoldAroundEdge(edge, false))
        )
      } else {
        List()
      }

    val singleRots = allSingleRotations(solution)

    wobbles ++ mirrors ++ rotations ++ moves ++ folds ++ singleRots ++ List(TransposeXY(), MirrorX(), MirrorY())
  }

  def correctOnce(solution: Vector[Point], options: Options): Vector[Point] = {
    val actions = possibleActions(solution, options)
    val sols = actions.map(a => Solution(a.apply(problem, solution), null))
    val results = sols.map(sol => validator.invalidnessMeasure(sol))
    //actions.zip(results).foreach { case (a, v) =>
    //  println(s"A: $a => $v")
    //}
    val bestIdx = results.zipWithIndex.minBy(_._1)._2
    sols(bestIdx).vertices
  }

  private def optimizerGoal(solution: Solution): BigInt = {
    evaluator.evaluate(solution) - validator.invalidnessMeasure(solution)
  }

  private def optimizeOnce(solution: Vector[Point], options: Options): Option[(Vector[Point], BigInt)] = {
    val actions = possibleActions(solution, options)
    val random = new Random()
    //println(s"A: $actions")
    val sols = actions.map(a => Solution(a.apply(problem, solution), null))
    val validSols = sols.flatMap(sol =>
      if (validator.validate(sol)) {
        Some(sol)
      } else if (options.correctByWobbles) {
        val wobbled = Solution(DumbSolver.wobbleAll(validator, random, sol.vertices, delta = options.wobbleDelta), null)
        Some(wobbled).filter(validator.validate)
      } else {
        None
      }
    )
    import scala.collection.parallel.CollectionConverters._
    val results = validSols.par.map(s => evaluator.evaluate(s))
    if (results.length >= 1) {
      val (bestResult, bestIdx) = results.zipWithIndex.minBy((p: (BigInt, Int)) => p._1)
      Some((validSols(bestIdx).vertices, bestResult))
    } else {
      println("No actions are available which would make solution valid")
      None
    }
  }

  def optimizeEagerly(solution: Vector[Point], options: Options): Vector[Point] = {
    var currentSolution = solution
    var prevResult = evaluator.evaluate(Solution(solution, null))
    var retryNumber = 0
    val maxRetries = 3
    var stop = false
    var i = 0

    while (! stop) {
      optimizeOnce(solution, options) match {
        case None => { stop = true }
        case Some((newSol, newRes)) => {
          currentSolution = newSol
          if (newRes <= prevResult) {
            retryNumber += 1
          }
          i += 1
          prevResult = newRes
          stop = (i > options.nIterations) || (retryNumber > maxRetries)
          }
      }
    }

    currentSolution
  }
}
