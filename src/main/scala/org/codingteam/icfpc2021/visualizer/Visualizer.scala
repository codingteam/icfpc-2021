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

    val offset_x = (hole.map(_.x).min).min(figure.vertices.map(_.x).min)
    val offset_y = (hole.map(_.y).min).min(figure.vertices.map(_.y).min)

    val max_x  = (hole.map(_.x - offset_x).max).max(figure.vertices.map(_.x - offset_x).max)
    val max_y  = (hole.map(_.y - offset_y).max).max(figure.vertices.map(_.y - offset_y).max)
    val max_coord = max_x.max(max_y)
    val scale: Int =
      if (max_coord < Int.MaxValue) {
        1
      } else {
        (max_coord / Int.MaxValue).toInt
      }

    val scaled_vertices =
      figure.vertices.map(p => ( ((p.x - offset_x) / scale).toInt, ((p.y - offset_y) / scale).toInt ))

    val holePoly = new Polygon(
      hole.map(p => ((p.x - offset_x) / scale).toInt).toArray,
      hole.map(p => ((p.y - offset_y) / scale).toInt).toArray,
      hole.size)

    new Frame {
      title = "visualizer"

      contents = Component.wrap(new JPanel() {
        override def paint(g: Graphics): Unit = {
          val g2 = g.asInstanceOf[Graphics2D]
          val imageOffset = 5
          val imageWidth = getWidth.toDouble - 2*imageOffset
          val imageHeight = getHeight.toDouble - 2*imageOffset
          val scaleX = imageWidth / (max_x - offset_x).toDouble
          val scaleY = imageHeight / (max_y - offset_y).toDouble
          val widgetScale = scaleX.min(scaleY)
          g2.scale(widgetScale, widgetScale)
          g2.translate(imageOffset, imageOffset)
          g2.draw(holePoly)

          g2.setColor(Color.RED)
          for (edge <- figure.edges) {
            val e1 = scaled_vertices(edge.vertex1)
            val e2 = scaled_vertices(edge.vertex2)
            g.drawLine(e1._1, e1._2, e2._1, e2._2)
          }

          g2.setColor(Color.BLACK)
          for (((x,y), i) <- scaled_vertices.zipWithIndex) {
            g.drawString(String.valueOf(i), x, y)
          }
        }
      })

      pack()
      centerOnScreen()
      open()
    }
  }
}
