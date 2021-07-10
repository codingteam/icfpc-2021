package org.codingteam.icfpc2021.som

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Point, Problem, Solution}

import scala.util.Random

class SOMOptimizer(
    problem: Problem,
    options: SOMOptimizer.Options = SOMOptimizer.Options(),
) {
  lazy val validator = new SolutionValidator(problem)

  def optimize(
      initialCoords: IndexedSeq[Point] = problem.figure.vertices,
  ): Option[Solution] = {
    val data = initialCoords.toArray

    def currentSolution = Solution(data.toVector)
    // TODO: implement this.

    Some(currentSolution).filter(validator.validate)
  }
}

object SOMOptimizer {
  case class Options(n: Integer = 1)

  val rnd = new Random

  def parallelOptimize(
      problem: Problem,
      options: Options,
      initials: IndexedSeq[IndexedSeq[Point]],
  ): IndexedSeq[Option[Solution]] = {
    import scala.collection.parallel.CollectionConverters._
    initials.par
      .map { init =>
        val opt = new SOMOptimizer(problem, options)
        opt.optimize(init)
      }
      .seq
      .toIndexedSeq
  }

  def randomInitialCoords(problem: Problem): IndexedSeq[Point] = {
    val rect = problem.holeRect
    val rectSize = rect.size
    for (_ <- 0 until problem.figureVerticesCount) yield {
      val x = rnd.nextDouble()
      val y = rnd.nextDouble()
      rect.min + Point(
        BigInt((rectSize.x.toDouble * x).toLong),
        BigInt((rectSize.y.toDouble * y).toLong),
      )
    }
  }

}
