package org.codingteam.icfpc2021.som

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.validator.SolutionValidator

import scala.collection.mutable
import scala.util.Random

class SOMSolver(problem: Problem,
                options: SOMSolver.Options = SOMSolver.Options()) {
  lazy val validator = new SolutionValidator(problem)
  val rnd = new Random()

  private def log(s: String): Unit = println(s)

  def optimize(initialCoords: IndexedSeq[Point] = problem.figure.vertices): Option[Solution] = {
    log(s"SOM solver, options=$options")
    val Scale: Int = 6
    val data = initialCoords.view.map(_.toPointBD(Scale)).toArray
    val problemData = problem.figure.vertices.view.map(_.toPointBD(Scale)).toArray
    val hole = problem.hole.view.map(_.toPointBD(Scale)).toArray

    def currentSolution = Solution(data.view.map(_.toPoint).toVector, null)

    def dataEdgeLengthSq(e: Edge): BigDecimal = data(e.vertex1) distanceSq data(e.vertex2)

    def srcEdgeLengthSq(e: Edge): BigDecimal = problemData(e.vertex1) distanceSq problemData(e.vertex2)

    val processedVertices = Array.ofDim[Boolean](problem.figure.vertices.size)
    val currentVertices = mutable.Buffer[Int]()
    val nextVertices = mutable.Buffer[Int]()
    val currentEdges = mutable.Buffer[Edge]()
    val nextEdges = mutable.Buffer[Edge]()

    for (t <- 0 until options.stepCount) {
      //      log(s"$t/${options.stepCount}..")
      java.util.Arrays.fill(processedVertices, false)
      val tf = (options.stepCount - t).toDouble / options.stepCount
      val alpha = options.startAlpha * tf
      val sigma = options.startSigma * tf
      val xt = hole(rnd.nextInt(problem.hole.size))
      val nearestInd = data.view.zipWithIndex.minBy(_._1 distanceSq xt)._2
      val neighbourSteps = (3.0 * sigma).round.toInt
      // Move toward to hole points.
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
      @inline def correctEdge(e: Edge): Unit = {
        val hci = alpha
        val v1 = data(e.vertex1)
        val v2 = data(e.vertex2)
        val actualLength = math.sqrt((v1 distanceSq v2).toDouble)
        val (minLength, maxLength) = {
          val (a, b) = problem.edgeDistRangeSqUnits(e.vertex1, e.vertex2)

          @inline def toDouble(v: BigInt) = math.sqrt(v.toDouble / 1000000)

          (toDouble(a), toDouble(b))
        }

        @inline def movePoints(k: Double): Unit = {
          data(e.vertex1) = v1.moveTowards(v2, k)
          data(e.vertex2) = v2.moveTowards(v1, k)
        }

        val Eps = 1e-6
        if (actualLength.abs < Eps) {
          if (minLength > 0) {
            val angle = rnd.nextDouble() * math.Pi * 2.0
            data(e.vertex1) = PointBD(math.cos(angle) * minLength * 0.5, math.cos(angle) * minLength * 0.5)
            data(e.vertex2) = PointBD.Zero - data(e.vertex1)
          }
        } else if (actualLength < minLength) {
          val d = minLength - actualLength
          val k = -d / actualLength * 0.5 * hci
          movePoints(k)
        } else if (actualLength > maxLength) {
          val d = actualLength - maxLength
          val k = d / actualLength * 0.5 * hci
          movePoints(k)
        }
      }

      val edge = problem.figure.edges(rnd.nextInt(problem.figure.edges.size))
      correctEdge(edge)
      // Move to minimize out of hole square (?).

    }
    //    data foreach (d => log(d.toString))
    //    currentSolution.vertices foreach (d => log(d.toString))
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