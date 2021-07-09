package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Json, Point, Problem}

import java.awt.{Color, Graphics, Graphics2D, Polygon, Dimension}
import java.nio.file.{Files, Path}
import javax.swing.JPanel
import scala.swing.{Component, Frame}

/** Translate between model coordinates (BigInt) and pixel screen coordinates (Int). */
class Translator(problem: Problem) {
  private val screenBorder = 8

  private var width = 200
  private var height = 200

  private val minX = (problem.hole.map(_.x).min).min(problem.figure.vertices.map(_.x).min)
  private val minY = (problem.hole.map(_.y).min).min(problem.figure.vertices.map(_.y).min)
  private val maxX = (problem.hole.map(_.x).max).max(problem.figure.vertices.map(_.x).max)
  private val maxY = (problem.hole.map(_.y).max).max(problem.figure.vertices.map(_.y).max)

  def setScreenDimensions(width: Int, height: Int) {
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

object Visualizer {
  def show(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val translator = new Translator(problem)

    val panel = new JPanel() {
        override def paint(g: Graphics): Unit = {
          val g2 = g.asInstanceOf[Graphics2D]
          translator.setScreenDimensions(getWidth, getHeight)

          g2.draw(translator.holeScreenPolygon)

          g2.setColor(Color.RED)
          for (edge <- problem.figure.edges) {
            val (x1, y1) = translator.toScreen(problem.figure.vertices(edge.vertex1))
            val (x2, y2) = translator.toScreen(problem.figure.vertices(edge.vertex2))
            g.drawLine(x1, y1, x2, y2)
          }

          g2.setColor(Color.BLACK)
          for ((vert, i) <- problem.figure.vertices.zipWithIndex) {
            val (x, y) = translator.toScreen(vert)
            g.drawString(String.valueOf(i), x, y)
          }
        }
      }
    panel.setPreferredSize(new Dimension(600, 400))

    new Frame {
      title = "visualizer"

      contents = Component.wrap(panel)

      pack()
      centerOnScreen()
      open()
    }
  }
}
