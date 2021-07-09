package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.{Json, Problem, Point, Rect}

import java.awt._
import java.awt.event.{ActionEvent, MouseEvent, MouseListener, _}
import java.nio.file.{Files, Path}
import javax.swing._
import scala.collection.mutable.BitSet

class Visualizer(val problem: Problem) extends JFrame("Codingteam ICPFC-2021") {

  private val translator = new Translator(problem)
  private var solution = problem.figure.vertices

  private var selection: Option[(java.awt.Point, java.awt.Point)] = None
  private var selectedFigureVertices = new BitSet

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
        g2.clearRect(0, 0, getWidth, getHeight)
        g2.draw(translator.holeScreenPolygon())

        g2.setColor(Color.GRAY)
        for ((vert, i) <- problem.hole.zipWithIndex) {
          val (x, y) = translator.toScreen(vert)
          g.drawString(String.valueOf(i), x, y)
        }

        selection foreach { sel =>
          val (x1, x2) = (sel._1.x.min(sel._2.x), sel._1.x.max(sel._2.x))
          val (y1, y2) = (sel._1.y.min(sel._2.y), sel._1.y.max(sel._2.y))
          g2.drawRect(x1, y1, x2 - x1, y2 - y1)
        }

        g2.setColor(Color.RED)
        for (i <- selectedFigureVertices) {
          val (x, y) = translator.toScreen(solution(i))
          g.fillOval(x - 4, y - 4, 8, 8)
        }

        g2.setColor(Color.RED)
        for (edge <- problem.figure.edges) {
          val (x1, y1) = translator.toScreen(solution(edge.vertex1))
          val (x2, y2) = translator.toScreen(solution(edge.vertex2))
          g.drawLine(x1, y1, x2, y2)
        }

        g2.setColor(Color.BLACK)
        for ((vert, i) <- solution.zipWithIndex) {
          val (x, y) = translator.toScreen(vert)
          g.drawString(String.valueOf(i), x, y)
        }
      }
    }
    p.setDoubleBuffered(true)
    p.setPreferredSize(new Dimension(600, 400))
    p.addMouseListener(new MouseListener() {
      override def mouseClicked(me: MouseEvent): Unit = {
        println(translator.toModel(me.getX, me.getY))
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mouseExited(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {
        selection = Some((new java.awt.Point(e.getX, e.getY), new java.awt.Point(e.getX, e.getY)))
        p.repaint()
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        selection = None
        p.repaint()
      }
    })

    p.addMouseMotionListener(new MouseMotionListener() {
      override def mouseDragged(e: MouseEvent): Unit = {
        selection foreach { sel =>
          sel._2.setLocation(e.getX, e.getY)

          val (x1, x2) = (sel._1.x.min(sel._2.x), sel._1.x.max(sel._2.x))
          val (y1, y2) = (sel._1.y.min(sel._2.y), sel._1.y.max(sel._2.y))
          val rect = Rect(translator.toModel(x1, y1), translator.toModel(x2, y2))

          selectedFigureVertices.clear()
          for((vert, i) <- solution.zipWithIndex) {
            if (rect.contains(vert)) {
              selectedFigureVertices.add(i)
            }
          }

          p.repaint()
        }
      }

      override def mouseMoved(e: MouseEvent): Unit = {}
    })
    p

  }

  private lazy val buttonsPanel = {
    val tb = new JToolBar()
    tb.add(visualizer.makeAction("Test Action", () => println("Test action called")))
    tb.add(visualizer.makeAction("←", () => moveSelected(Point(-1, 0))))
    tb.add(visualizer.makeAction("→", () => moveSelected(Point(+1, 0))))
    tb.add(visualizer.makeAction("↑", () => moveSelected(Point(0, -1))))
    tb.add(visualizer.makeAction("↓", () => moveSelected(Point(0, +1))))
    tb
  }

  private def moveSelected(delta: Point): Unit = {
    solution = solution.zipWithIndex.map { case (p, idx) =>
      if (selectedFigureVertices.contains(idx)) p + delta else p
    }
    problemPanel.repaint()
  }

  private def init(): Unit = {
    getContentPane.add(mainPanel)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocationByPlatform(true)
    pack()
  }

  init()
}

package object visualizer {
  // TODO: move this helper function somewhere to an appropriate place
  def makeAction(name: String, callback: () => Unit): AbstractAction = new AbstractAction(name) {
    override def actionPerformed(e: ActionEvent): Unit = callback()
  }
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
