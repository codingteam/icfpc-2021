package org.codingteam.icfpc2021.som

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Point, Problem, Solution}

import scala.collection.mutable
import scala.util.Random

class SOMSolver(problem: Problem,
                options: SOMSolver.Options = SOMSolver.Options()) {
  lazy val validator = new SolutionValidator(problem)
  val rnd = new Random()

  private def log(s: String): Unit = println(s)

  def optimize(initialCoords: IndexedSeq[Point] = problem.figure.vertices): Option[Solution] = {
    log(s"SOM solver, options=$options")
    val data = initialCoords.toArray

    def currentSolution = Solution(data.toVector, null)

    val processedVertices = Array.ofDim[Boolean](problem.figure.vertices.size)
    val currentVertices = mutable.Buffer[Int]()
    val nextVertices = mutable.Buffer[Int]()
    for (t <- 0 until options.stepCount) {
      log(s"$t/${options.stepCount}..")
      java.util.Arrays.fill(processedVertices, false)
      val xt = problem.hole(rnd.nextInt(problem.hole.size))
      val tf = (options.stepCount - t).toDouble / options.stepCount
      val alpha = options.startAlpha * tf
      val sigma = options.startSigma * tf
      // Move toward to hole points.
      val nearestInd = data.view.zipWithIndex.minBy(_._1 distanceSq xt)._2
      val neighbourSteps = (3.0 * sigma).round.toInt
      currentVertices.clear()
      currentVertices += nearestInd
      for (i <- 0 to neighbourSteps) {
        val hci = alpha * math.exp(-i.toDouble / (2 * sigma * sigma))
        for (vInd <- currentVertices) {
          // Move neighbour [vInd] towards hole point xt.
          data(vInd) = data(vInd).moveTowards(xt, hci)
        }
        // find next neighbours.
        nextVertices.clear()
        for (vInd <- currentVertices) {
          processedVertices(vInd) = true
          for (nInd <- problem.figure.vertexNeighbours(vInd) if !processedVertices(nInd)) {
            nextVertices += nInd
          }
        }
        currentVertices.clear()
        nextVertices foreach currentVertices.+=
      }
      // Move to correct edges.

      // Move to minimize out of hole square (?).

    }
    Some(currentSolution)
  }
}

object SOMSolver {
  case class Options(stepCount: Integer = 100000,
                     startSigma: Double = 1.0,
                     startAlpha: Double = 0.9)

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