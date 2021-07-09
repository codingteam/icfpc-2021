package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Point, Problem}
import java.awt.Polygon

/** Translate between model coordinates (BigInt) and pixel screen coordinates (Int). */
class Translator(problem: Problem) {
  private val screenBorder = 8

  private var width = 200
  private var height = 200

  private val minX = (problem.hole.map(_.x).min).min(problem.figure.vertices.map(_.x).min)
  private val minY = (problem.hole.map(_.y).min).min(problem.figure.vertices.map(_.y).min)
  private val maxX = (problem.hole.map(_.x).max).max(problem.figure.vertices.map(_.x).max)
  private val maxY = (problem.hole.map(_.y).max).max(problem.figure.vertices.map(_.y).max)

  def setScreenDimensions(width: Int, height: Int): Unit = {
    this.width = width.max(screenBorder * 2 + 1)
    this.height = height.max(screenBorder * 2 + 1)
  }

  def toScreen(p: Point): (Int, Int) = (toScreenX(p.x), toScreenY(p.y))
  // TODO: preserve aspect ratio
  def toScreenX(x: BigInt): Int = ((x - minX) * (width - screenBorder * 2 ) / (maxX - minX) + screenBorder).toInt
  def toScreenY(y: BigInt): Int = ((y - minY) * (height - screenBorder * 2 )  / (maxY - minY) + screenBorder).toInt

  def toModel(x: Int, y: Int): Point = ???

  def holeScreenPolygon(): Polygon = new Polygon(
    problem.hole.map(p => toScreenX(p.x)).toArray,
    problem.hole.map(p => toScreenY(p.y)).toArray,
    problem.hole.size,
  )
}

