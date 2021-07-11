package org.codingteam.icfpc2021.validator

import org.codingteam.icfpc2021._

import java.awt.image.BufferedImage
import java.awt.{Color, Polygon}
import java.nio.file.{Files, Path}
import scala.swing.Graphics2D

object EdgeCheckResult extends Enumeration {
  type EdgeCheckResult = Value
  val TooShort, Exact, TooLong = Value
}

import org.codingteam.icfpc2021.validator.EdgeCheckResult._

class SolutionValidator(problem: Problem) {
  private val ClearColor: Color = Color.BLACK
  private val FillColor: Color = Color.WHITE
  private lazy val (shiftX, shiftY, scaleX, scaleY, imgSizeX, imgSizeY) = {
    require(problem.hole.nonEmpty, "Hole points list must be not empty")
    val MaxImageSize = 32 * 1024
    val Point(shiftX, shiftY) = problem.holeRect.min
    val Point(sizeX, sizeY) = problem.holeRect.size
    val (imgSizeX, scaleX) = if (sizeX > MaxImageSize)
      (MaxImageSize, MaxImageSize.toDouble / sizeX.toDouble)
    else
      (sizeX.toInt, 1.0)

    val (imgSizeY, scaleY) = if (sizeX > MaxImageSize)
      (MaxImageSize, MaxImageSize.toDouble / sizeY.toDouble)
    else
      (sizeY.toInt, 1.0)

    (shiftX, shiftY, scaleX, scaleY, imgSizeX, imgSizeY)
  }
  private lazy val (holeImageArray, figureImage, figureImageArray) = {
    require(problem.hole.nonEmpty, "Hole points list must be not empty")
    val figureImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
    val figureImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)

    val holeImage = new BufferedImage(imgSizeX, imgSizeY, BufferedImage.TYPE_INT_RGB)
    val g2 = holeImage.getGraphics.asInstanceOf[Graphics2D]
    g2.setColor(ClearColor)
    g2.fillRect(0, 0, imgSizeX, imgSizeY)
    val (holeXs, holeYs, holeLength) = splitPointsIntoCoords(problem.hole map pointToImageCoord)
    g2.setColor(FillColor)
    g2.drawPolygon(holeXs, holeYs, holeLength)
    g2.fillPolygon(holeXs, holeYs, holeLength)
    val holeImageArray = Array.ofDim[Int](imgSizeX * imgSizeY)
    holeImage.getRGB(0, 0, imgSizeX, imgSizeY, holeImageArray, 0, imgSizeX)
    (holeImageArray, figureImage, figureImageArray)
  }

  private def pointToImageCoord(p: Point): Point = {
    Point(((p.x - shiftX).toDouble * scaleX).toInt, ((p.y - shiftY).toDouble * scaleY).toInt)
  }

  private def splitPointsIntoCoords(ps: Seq[Point]) = {
    (ps.view.map(_.x.toInt).toArray, ps.view.map(_.y.toInt).toArray, ps.size)
  }

  def isPointInHole(p: Point): Boolean = {
    val hole = new Polygon(problem.hole.map(_.x.intValue).toArray, problem.hole.map(_.y.intValue).toArray, problem.hole.size)
    hole.contains(p.x.toDouble, p.y.toDouble)
  }

  def isPointInHole(p: PointD): Boolean = {
    val hole = new Polygon(problem.hole.map(_.x.intValue).toArray, problem.hole.map(_.y.intValue).toArray, problem.hole.size)
    hole.contains(p.x, p.y)
  }

  def invalidnessMeasure(solution: Solution) : BigInt = {
    val edgeDeltas = problem.figure.edges.map(e => calcEdgeLengthDelta(solution, e)._1.abs).sum
    val pointsOutside = solution.vertices.count(isPointInHole)
    10 * pointsOutside + edgeDeltas
  }

  def validate(solution: Solution): Boolean = {
    require(solution.vertices.size == problem.figure.vertices.size, "Wrong vertices array size")
    validateEdgeLength(solution) && !hasPointsOutsideOfImage(solution) && validateHole(solution)
  }

  def calcEdgeLengthDelta(solution: Solution, e: Edge): (BigInt, BigInt) = {
    val problemDist = problem.figure.vertices(e.vertex1) distanceSq problem.figure.vertices(e.vertex2)
    val solutionDist = solution.vertices(e.vertex1) distanceSq solution.vertices(e.vertex2)
    // d' in solution, d in problem.
    // abs(d' / d - 1) <= eps / 1000000
    // abs((d' - d) / d) <= eps / 1000000
    // abs((d' - d) ) * 1000000 <= d * eps
    val delta = solutionDist - problemDist
    (delta, problemDist)
  }

  def checkEdgeLength(solution: Solution, e: Edge): EdgeCheckResult = {
    val K = BigInt(1000000)
    val (delta, problemDist) = calcEdgeLengthDelta(solution, e)
    val correctLength = delta.abs * K <= problemDist * problem.epsilon
    if (correctLength) {
      Exact
    } else if (delta > 0) {
      TooLong
    } else {
      TooShort
    }
  }

  def validateEdgeLength(solution: Solution, vertIndex: Option[Int] = None): Boolean = {
    problem.figure.edges forall { e =>
      vertIndex match {
        case None => checkEdgeLength(solution, e) == Exact
        case Some(idx) =>
          if (e.vertex1 == idx || e.vertex2 == idx) {
            checkEdgeLength(solution, e) == Exact
          }  else {
            true
          }
      }

    }
  }

  def hasPointsOutsideOfImage(solution: Solution): Boolean = {
    problem.figure.edges exists { e =>
      val v1 = pointToImageCoord(solution.vertices(e.vertex1))
      val v2 = pointToImageCoord(solution.vertices(e.vertex2))
      pointIsOutsideOfImage(v1) || pointIsOutsideOfImage(v2)
    }
  }

  private def pointIsOutsideOfImage(v: Point) = v.x < 0 || v.y < 0 || v.x >= imgSizeX || v.y >= imgSizeY

  private def drawSolutionToFigure(solution: Solution): Unit = {
    val g2 = figureImage.getGraphics.asInstanceOf[Graphics2D]
    g2.setColor(ClearColor)
    g2.fillRect(0, 0, imgSizeX, imgSizeY)
    g2.setColor(FillColor)
    for (e <- problem.figure.edges) {
      val v1 = pointToImageCoord(solution.vertices(e.vertex1))
      val v2 = pointToImageCoord(solution.vertices(e.vertex2))
      g2.drawLine(v1.x.toInt, v1.y.toInt, v2.x.toInt, v2.y.toInt)
    }
    figureImage.getRGB(0, 0, imgSizeX, imgSizeY, figureImageArray, 0, imgSizeX)
  }

  def validateHole(solution: Solution): Boolean = {
    solution.vertices.forall(problem.isPointInHoleExact) && problem.figure.edges.forall(edge => {
      val v1 = solution.vertices(edge.vertex1)
      val v2 = solution.vertices(edge.vertex2)
      !problem.segmentGoesOutsideTheHole((v1, v2))
    })
  }

  /**
   * Draw solution and return outside-hole pixels count.
   * Edges (not filled polygons) are drawn.
   * Points outside of image aren't counted.
   *
   * @param solution solution to check.
   * @return inaccurate pixel count.
   */
  def getPixelCountOutsideHole(solution: Solution): Int = {
    drawSolutionToFigure(solution)
    // compare raster arrays.
    val clearColorRGB = ClearColor.getRGB
    figureImageArray.indices count { i =>
      figureImageArray(i) != clearColorRGB && holeImageArray(i) == clearColorRGB
    }
  }

}

object SolutionValidator {
  def validateFile(problemFile: Path, solutionFile: Path): Unit = {
    val problem = Json.parseProblem(Files.readString(problemFile))
    val solution = Json.parseSolution(Files.readString(solutionFile))
    val validator = new SolutionValidator(problem)
    if (validator.validate(solution)) {
      println("Ok")
    } else {
      println("Fail")
    }
  }
}