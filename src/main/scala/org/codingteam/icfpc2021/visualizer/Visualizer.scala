package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.Json

import java.awt.{Color, Graphics, Graphics2D, Polygon}
import java.nio.file.{Files, Path}
import javax.swing.JPanel
import scala.swing.{Component, Frame}

object Visualizer {
  def show(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val hole = problem.hole
    val figure = problem.figure
    val holePoly = new Polygon(hole.map(_.x).toArray, hole.map(_.y).toArray, hole.size)

    new Frame {
      title = "visualizer"

      contents = Component.wrap(new JPanel() {
        override def paint(g: Graphics): Unit = {
          val g2 = g.asInstanceOf[Graphics2D]
          g2.draw(holePoly)

          g2.setColor(Color.RED)
          for (edge <- figure.edges) {
            val e1 = figure.vertices(edge.vertex1)
            val e2 = figure.vertices(edge.vertex2)
            g.drawLine(e1.x, e1.y, e2.x, e2.y)
          }
        }
      })

      pack()
      centerOnScreen()
      open()
    }
  }
}
