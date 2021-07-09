package org.codingteam.icfpc2021.validator

import org.codingteam.icfpc2021.{Json, Point, Problem, Solution}

import java.awt.Color
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import scala.swing.Graphics2D

class SolutionValidator(problem: Problem) {
  private val ClearColor: Color = Color.BLACK
  private val FillColor: Color = Color.WHITE
  private lazy val (shiftX, shiftY, scaleX, scaleY, imgSizeX, imgSizeY) = {
    require(problem.hole.nonEmpty, "Hole points list must be not empty")
    val MaxImageSize = 4 * 1024
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

  def validate(solution: Solution): Boolean = {
    require(solution.vertices.size == problem.figure.vertices.size, "Wrong vertices array size")
    validateEdgeLength(solution) && validateHole(solution)
  }

  def validateEdgeLength(solution: Solution): Boolean = {
    val K = BigInt(1000000)
    problem.figure.edges forall { e =>
      val problemDist = problem.figure.vertices(e.vertex1) distanceSq problem.figure.vertices(e.vertex2)
      val solutionDist = solution.vertices(e.vertex1) distanceSq solution.vertices(e.vertex2)
      // d' in solution, d in problem.
      // abs(d' / d - 1) <= eps / 1000000
      // abs((d' - d) / d) <= eps / 1000000
      // abs((d' - d) ) * 1000000 <= d * eps
      val correctLength = (solutionDist - problemDist).abs * K <= problemDist * problem.epsilon
      correctLength
    }
  }

  private def pointIsOutsideImage(v: Point) = v.x < 0 || v.y < 0 || v.x >= imgSizeX || v.y >= imgSizeY

  def validateHole(solution: Solution): Boolean = {
    val g2 = figureImage.getGraphics.asInstanceOf[Graphics2D]
    g2.setColor(ClearColor)
    g2.fillRect(0, 0, imgSizeX, imgSizeY)
    g2.setColor(FillColor)
    for (e <- problem.figure.edges) {
      val v1 = pointToImageCoord(solution.vertices(e.vertex1))
      val v2 = pointToImageCoord(solution.vertices(e.vertex2))
      if (pointIsOutsideImage(v1) || pointIsOutsideImage(v2))
        return false
      g2.drawLine(v1.x.toInt, v1.y.toInt, v2.x.toInt, v2.y.toInt)
    }
    figureImage.getRGB(0, 0, imgSizeX, imgSizeY, figureImageArray, 0, imgSizeX)
    // compare raster arrays.
    val clearColorRGB = ClearColor.getRGB
    for (i <- figureImageArray.indices) {
      if (figureImageArray(i) != clearColorRGB && holeImageArray(i) == clearColorRGB)
        return false
    }
    true
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