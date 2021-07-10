package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Point, Problem}
import java.awt.Polygon

/** Translate between model coordinates (BigInt) and pixel screen coordinates
  * (Int).
  */
class Translator(problem: Problem) {

  private val minX =
    (problem.hole.map(_.x).min).min(problem.figure.vertices.map(_.x).min)
  private val minY =
    (problem.hole.map(_.y).min).min(problem.figure.vertices.map(_.y).min)
  private val maxX =
    (problem.hole.map(_.x).max).max(problem.figure.vertices.map(_.x).max)
  private val maxY =
    (problem.hole.map(_.y).max).max(problem.figure.vertices.map(_.y).max)

  private var scale1 = BigInt(1)
  private var scale2 = BigInt(1)
  private var shiftX = 0
  private var shiftY = 0

  def setScreenDimensions(newWidth: Int, newHeight: Int): Unit = {
    val screenBorder = 8

    val width = newWidth.max(screenBorder * 2 + 1)
    val height = newHeight.max(screenBorder * 2 + 1)

    if (
      (width - screenBorder * 2) * (maxY - minY) < (height - screenBorder * 2) * (maxX - minY)
    ) {
      scale1 = width - screenBorder * 2
      scale2 = maxX - minX
      shiftY = ((height - (maxY - minY) * scale1 / scale2) / 2).toInt
      shiftX = screenBorder
    } else {
      scale1 = height - screenBorder * 2
      scale2 = maxY - minY
      shiftX = ((width - (maxX - minX) * scale1 / scale2) / 2).toInt
      shiftY = screenBorder
    }
  }

  def toScreen(p: Point): (Int, Int) = (toScreenX(p.x), toScreenY(p.y))
  def toScreenX(x: BigInt): Int = ((x - minX) * scale1 / scale2).toInt + shiftX
  def toScreenY(y: BigInt): Int = ((y - minY) * scale1 / scale2).toInt + shiftY

  def toModel(x: Int, y: Int): Point = Point(toModelX(x), toModelY(y))
  def toModelX(x: Int): BigInt = (x - shiftX) * scale2 / scale1 + minX
  def toModelY(y: Int): BigInt = (y - shiftY) * scale2 / scale1 + minY

  def holeScreenPolygon(): Polygon = new Polygon(
    problem.hole.map(p => toScreenX(p.x)).toArray,
    problem.hole.map(p => toScreenY(p.y)).toArray,
    problem.hole.size,
  )
}
