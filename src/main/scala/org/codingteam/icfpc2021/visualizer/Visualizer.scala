package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Json, Point, Problem}

import java.awt._
import java.awt.event._
import java.nio.file.{Files, Path}
import javax.swing.{JPanel, WindowConstants}
import scala.swing.{Component, Frame}

object Visualizer {
  def show(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val translator = new Translator(problem)

    var selection: Option[(java.awt.Point, java.awt.Point)] = None

    val panel = new JPanel() {
        override def paint(g: Graphics): Unit = {
          super.paint(g);
          val g2 = g.asInstanceOf[Graphics2D]
          translator.setScreenDimensions(getWidth, getHeight)

          g2.draw(translator.holeScreenPolygon())

          g2.setColor(Color.GRAY)
          for ((vert, i) <- problem.hole.zipWithIndex) {
            val (x, y) = translator.toScreen(vert)
            g.drawString(String.valueOf(i), x, y)
          }

          selection match {
            case Some(r) => {
              val (x1, x2) = (r._1.x.min(r._2.x), r._1.x.max(r._2.x))
              val (y1, y2) = (r._1.y.min(r._2.y), r._1.y.max(r._2.y))
              g2.drawRect(x1, y1, x2 - x1, y2 - y1)
            }
            case None => {}
          }

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

    panel.addMouseListener(new MouseListener() {
      override def mouseClicked(me: MouseEvent): Unit = {
        println(translator.toModel(me.getX, me.getY))
      }
      override def mouseEntered(e: MouseEvent): Unit = {}
      override def mouseExited(e: MouseEvent): Unit = {}
      override def mousePressed(e: MouseEvent): Unit = {
        selection = Some((new java.awt.Point(e.getX, e.getY), new java.awt.Point(e.getX, e.getY)))
        panel.repaint()
      }
      override def mouseReleased(e: MouseEvent): Unit = {
        selection = None
        panel.repaint()
      }
    })

    panel.addMouseMotionListener(new MouseMotionListener() {
      override def mouseDragged(e: MouseEvent): Unit = {
        selection match {
          case Some(sel) => {
            sel._2.setLocation(e.getX, e.getY)
            panel.repaint()
          }
          case None => {}
        }
      }
      override def mouseMoved(e: MouseEvent): Unit = {}
    })

    new Frame {
      title = "visualizer"

      contents = Component.wrap(panel)

      peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      pack()
      centerOnScreen()
      open()
    }
  }
}
