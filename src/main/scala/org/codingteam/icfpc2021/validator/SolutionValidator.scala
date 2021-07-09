package org.codingteam.icfpc2021.validator

import org.codingteam.icfpc2021.{Point, Problem, Solution}

import java.awt.Color
import java.awt.image.BufferedImage
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
    // compare raster arrays.
    val clearColorRGB = ClearColor.getRGB
    for (i <- figureImageArray.indices) {
      if (figureImageArray(i) != clearColorRGB && holeImageArray(i) == clearColorRGB)
        return false
    }
    true
  }

}
