package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Json, Problem}

import java.awt._
import java.awt.event.{ActionEvent, MouseEvent, MouseListener}
import java.nio.file.{Files, Path}
import javax.swing._

class Visualizer(val problem: Problem) extends JFrame("Codingteam ICPFC-2021") {

  private val translator = new Translator(problem)

  private lazy val mainPanel = {
    val p = new JPanel()
    p.setLayout(new BorderLayout())
    p.add(problemPanel, BorderLayout.CENTER)
    p.add(buttonsPanel, BorderLayout.NORTH)
    p
  }
  private lazy val problemPanel = {
    val p = new JPanel() {
      override def paint(g: Graphics): Unit = {
        val g2 = g.asInstanceOf[Graphics2D]
        translator.setScreenDimensions(getWidth, getHeight)

        g2.draw(translator.holeScreenPolygon())

        g2.setColor(Color.GRAY)
        for ((vert, i) <- problem.hole.zipWithIndex) {
          val (x, y) = translator.toScreen(vert)
          g.drawString(String.valueOf(i), x, y)
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
    p.setPreferredSize(new Dimension(600, 400))
    p.addMouseListener(new MouseListener() {
      override def mouseClicked(me: MouseEvent): Unit = {
        println(translator.toModel(me.getX, me.getY))
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mouseExited(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {}

      override def mouseReleased(e: MouseEvent): Unit = {}
    })
    p

  }
  private lazy val buttonsPanel = {
    val tb = new JToolBar()
    tb.add(testAction)
    tb
  }
  private lazy val testAction = new AbstractAction("Test action") {
    override def actionPerformed(e: ActionEvent): Unit = println("Test action called")
  }

  private def init(): Unit = {
    getContentPane.add(mainPanel)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocationByPlatform(true)
    pack()
  }

  init()
}

object Visualizer {
  def show(problemFile: Path): Visualizer = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val v = new Visualizer(problem)
    v.setVisible(true)
    v
  }
}
