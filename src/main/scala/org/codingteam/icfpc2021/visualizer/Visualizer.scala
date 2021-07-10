package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021._

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{BorderLayout, Color, Dimension, Graphics}
import java.nio.file.{Files, Path}
import javax.swing._
import scala.collection.mutable
import scala.swing.Graphics2D

class Visualizer(val problem: Problem) extends JFrame("Codingteam ICPFC-2021") {

  private val translator = new Translator(problem)
  private var solution = problem.figure.vertices
  private val originalEdgeLengths = problem.figure.edges.map(e => solution(e.vertex1) distanceSq solution(e.vertex2))

  trait Tool {
    // TODO: decouple default implementations from childrens implementations
    //       i.e. childs shouldn't call `super.something()`

    var rect: Option[(java.awt.Point, java.awt.Point)] = None

    def startDrag(e: MouseEvent): Unit = {
      rect = Some((new java.awt.Point(e.getX, e.getY), new java.awt.Point(e.getX, e.getY)))
      vizRepaint()
    }

    def endDrag(): Unit = {
      rect = None
      vizRepaint()
    }

    def dragged(e: MouseEvent): Unit = {
      rect foreach { sel =>
        sel._2.setLocation(e.getX, e.getY)
      }
    }
  }

  class SelectionTool extends Tool {
    var selectedFigureVertices: mutable.BitSet = mutable.BitSet()

    override def dragged(e: MouseEvent): Unit = {
      super.dragged(e)
      rect foreach { sel =>
        val (x1, x2) = (sel._1.x.min(sel._2.x), sel._1.x.max(sel._2.x))
        val (y1, y2) = (sel._1.y.min(sel._2.y), sel._1.y.max(sel._2.y))
        val rect = Rect(translator.toModel(x1, y1), translator.toModel(x2, y2))

        selectedFigureVertices.clear()
        for ((vert, i) <- solution.zipWithIndex) {
          if (rect.contains(vert)) {
            selectedFigureVertices.add(i)
          }
        }

        vizRepaint()
      }
    }
  }

  class MoveTool extends Tool {
    var prev: Point = Point(0, 0)

    override def startDrag(e: MouseEvent): Unit = {
      super.startDrag(e)
      prev = Point(0, 0)
    }

    override def dragged(e: MouseEvent): Unit = {
      super.dragged(e)
      rect foreach { sel =>
        val curr = translator.toModelDelta(sel._2.x - sel._1.x, sel._2.y - sel._1.y)

        if (curr != prev) {
          moveSelected(curr - prev)
        }
        prev = curr
      }
    }
  }

  private val selectionTool = new SelectionTool
  private val moveTool = new MoveTool
  private var tool = new Tool() {}

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
        super.paint(g)
        val g2 = g.asInstanceOf[Graphics2D]
        translator.setScreenDimensions(getWidth, getHeight)
        g2.draw(translator.holeScreenPolygon())

        g2.setColor(Color.GRAY)
        for ((vert, i) <- problem.hole.zipWithIndex) {
          val (x, y) = translator.toScreen(vert)
          g.drawString(String.valueOf(i), x, y)
        }

        selectionTool.rect foreach { sel =>
          val (x1, x2) = (sel._1.x.min(sel._2.x), sel._1.x.max(sel._2.x))
          val (y1, y2) = (sel._1.y.min(sel._2.y), sel._1.y.max(sel._2.y))
          g2.drawRect(x1, y1, x2 - x1, y2 - y1)
        }

        g2.setColor(Color.RED)
        for (i <- selectionTool.selectedFigureVertices) {
          val (x, y) = translator.toScreen(solution(i))
          g.fillOval(x - 4, y - 4, 8, 8)
        }

        for ((edge, i) <- problem.figure.edges.zipWithIndex) {
          val (x1, y1) = translator.toScreen(solution(edge.vertex1))
          val (x2, y2) = translator.toScreen(solution(edge.vertex2))
          val distance = solution(edge.vertex1) distanceSq solution(edge.vertex2)
          val origDistance = originalEdgeLengths(i)
          if (distance > origDistance) {
            g2.setColor(Color.GREEN)
          } else if (distance < origDistance) {
            g2.setColor(Color.BLUE)
          } else {
            g2.setColor(Color.RED)
          }

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

      override def mousePressed(e: MouseEvent): Unit = tool.startDrag(e)

      override def mouseReleased(e: MouseEvent): Unit = tool.endDrag()
    })

    p.addMouseMotionListener(new MouseMotionListener() {
      override def mouseDragged(e: MouseEvent): Unit = tool.dragged(e)

      override def mouseMoved(e: MouseEvent): Unit = {}
    })

    p
  }

  private lazy val buttonsPanel = {
    val tb = new JToolBar()
    tb.add(makeAction("Print JSON", () => printSolution()))
    tb.add(makeAction("Validate", () => validateSolution()))

    // Move
    tb.add(makeAction("←", () => moveSelected(Point(-1, 0))))
    tb.add(makeAction("→", () => moveSelected(Point(+1, 0))))
    tb.add(makeAction("↑", () => moveSelected(Point(0, -1))))
    tb.add(makeAction("↓", () => moveSelected(Point(0, +1))))

    // Tools
    val buttonGroup = new ButtonGroup()

    def addTool(text: String, newTool: Tool): Unit = {
      var button = new JToggleButton(text)
      tb.add(button)
      buttonGroup.add(button)
      button.addActionListener(e => {
        tool.endDrag()
        tool = newTool
      })
    }

    addTool("Select", selectionTool)
    addTool("Move", moveTool)

    tb
  }

  private def moveSelected(delta: Point): Unit = {
    solution = solution.zipWithIndex.map { case (p, idx) =>
      if (selectionTool.selectedFigureVertices.contains(idx)) p + delta else p
    }
    problemPanel.repaint()
  }

  private def printSolution() : Unit = {
    val sol = Solution(solution)
    println(Json.serializeSolution(sol))
  }

  private def validateSolution() : Unit = {
    val sol = Solution(solution)
    val validator = new SolutionValidator(problem)
    if (validator.validate(sol)) {
      println("Ok")
    } else {
      println("Fail")
    }
  }

  private def init(): Unit = {
    getContentPane.add(mainPanel)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocationByPlatform(true)
    pack()
  }

  private def vizRepaint(): Unit = {
    problemPanel.repaint()
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
