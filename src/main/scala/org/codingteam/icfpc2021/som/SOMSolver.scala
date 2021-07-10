package org.codingteam.icfpc2021.som

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Point, Problem, Solution}

import scala.util.Random

class SOMSolver(problem: Problem,
                options: SOMSolver.Options = SOMSolver.Options()) {
  lazy val validator = new SolutionValidator(problem)
  val rnd = new Random()

  def optimize(initialCoords: IndexedSeq[Point] = problem.figure.vertices): Option[Solution] = {
    val data = initialCoords.toArray

    def currentSolution = Solution(data.toVector)
    // TODO: implement this.
    for (t <- 0 until options.stepCount) {
      val xt = problem.hole(rnd.nextInt(problem.hole.size))
      val tf = (options.stepCount - t).toDouble / options.stepCount
      val alpha = options.startAlpha * tf
      val sigma = options.startSigma * tf
      // Move toward to hole points.
      val (nearestPoint, nearestInd) = data.view.zipWithIndex.minBy(_._1 distanceSq xt)
      //      val hci = alpha * math.exp(-(xt distanceSq neighbour).toDouble / (2 * sigma * sigma))

      // Move to correct edges.

      // Move to minimize out of hole square (?).
    }
    Some(currentSolution)
  }
}

object SOMSolver {
  case class Options(stepCount: Integer = 100000,
                     startSigma: Double = 1.0,
                     startAlpha: Double = 0.1)

  val rnd = new Random

  def parallelOptimize(problem: Problem,
                       options: Options,
                       initials: IndexedSeq[IndexedSeq[Point]]): IndexedSeq[Option[Solution]] = {
    import scala.collection.parallel.CollectionConverters._
    initials.par.map { init =>
      val opt = new SOMSolver(problem, options)
      opt.optimize(init)
    }.seq.toIndexedSeq
  }

  def randomInitialCoords(problem: Problem): IndexedSeq[Point] = {
    val rect = problem.holeRect
    val rectSize = rect.size
    for (_ <- 0 until problem.figureVerticesCount) yield {
      val x = rnd.nextDouble()
      val y = rnd.nextDouble()
      rect.min + Point(BigInt((rectSize.x.toDouble * x).toLong), BigInt((rectSize.y.toDouble * y).toLong))
    }
  }

}