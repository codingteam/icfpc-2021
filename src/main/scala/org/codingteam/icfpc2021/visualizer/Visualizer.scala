package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Json, Point, Problem}

import java.awt._
import java.nio.file.{Files, Path}
import javax.swing.JPanel
import scala.swing.{Component, Frame}

object Visualizer {
  def show(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val translator = new Translator(problem)

    val panel = new JPanel() {
        override def paint(g: Graphics): Unit = {
          val g2 = g.asInstanceOf[Graphics2D]
          translator.setScreenDimensions(getWidth, getHeight)

          g2.draw(translator.holeScreenPolygon())

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
