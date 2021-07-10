package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.force_solver.ForceBasedSolver
import org.codingteam.icfpc2021.solver.DumbSolver
import org.codingteam.icfpc2021.som.{SOMSolver, SOMSolverOptionsPanel}
import org.codingteam.icfpc2021.validator.{EdgeCheckResult, SolutionValidator}

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{BorderLayout, Color, Dimension, Graphics}
import java.nio.file.{Files, Path}
import javax.swing._
import scala.collection.mutable
import scala.language.implicitConversions
import scala.swing.Graphics2D
import scala.util.chaining._

class Visualizer(var problemFile: Path, var problem: Problem) extends JFrame("Codingteam ICPFC-2021") {

  private var translator: Translator = _
  private var solution: Vector[Point] = _
  private var originalEdgeLengths: Vector[BigInt] = _

  trait Tool {
    // TODO: decouple default implementations from childrens implementations
    //       i.e. childs shouldn't call `super.something()`

    var rect: Option[(java.awt.Point, java.awt.Point)] = None

    def startDrag(e: MouseEvent): Unit = {
      rect = Some((new java.awt.Point(e.getX, e.getY), new java.awt.Point(e.getX, e.getY)))
      updateStatus()
    }

    def endDrag(): Unit = {
      rect = None
      updateStatus()
    }

    def dragged(e: MouseEvent): Unit = {
      rect foreach { sel =>
        sel._2.setLocation(e.getX, e.getY)
      }
    }

    def reset(): Unit = {}
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

        updateStatus()
      }
    }

    override def reset(): Unit = selectedFigureVertices = mutable.BitSet()
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

    override def reset(): Unit = prev = Point(0, 0)
  }

  private val selectionTool = new SelectionTool
  private val moveTool = new MoveTool
  private var tool = new Tool() {}

  private lazy val mainPanel = {
    val p = new JPanel()
    p.setLayout(new BorderLayout())
    p.add(problemPanel, BorderLayout.CENTER)
    p.add(buttonsPanel, BorderLayout.NORTH)
    p.add(statusPanel, BorderLayout.SOUTH)
    p.add(somOptionsPanel, BorderLayout.EAST)
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

        val validator = new SolutionValidator(problem)
        for ((edge, i) <- problem.figure.edges.zipWithIndex) {
          val (x1, y1) = translator.toScreen(solution(edge.vertex1))
          val (x2, y2) = translator.toScreen(solution(edge.vertex2))
          val distance = solution(edge.vertex1) distanceSq solution(edge.vertex2)
          val origDistance = originalEdgeLengths(i)
          val check = validator.checkEdgeLength(Solution(solution, null), edge)
          if (check == EdgeCheckResult.TooLong) {
            g2.setColor(Color.GREEN)
          } else if (check == EdgeCheckResult.TooShort) {
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
    p.setPreferredSize(new Dimension(1920, 960))

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

  private def moveToNextProblem(dir: Int): Unit = {
    val problemDir = problemFile.getParent
    val currentFileName = problemFile.getFileName.toString
    val problemNumber = currentFileName.replace(".json", "").toInt
    val newFile = problemDir.resolve(s"${problemNumber + dir}.json")
    if (Files.exists(newFile)) {
      problemFile = newFile
      problem = Json.parseProblem(Files.readString(problemFile))
      init()
    }
  }

  private lazy val buttonsPanel = {
    val tb = new JToolBar()
    tb.add(makeAction("Prev file", () => moveToNextProblem(-1)))
    tb.add(makeAction("Next file", () => moveToNextProblem(1)))

    tb.add(makeAction("Print JSON", () => printSolution()))

    // Move
    tb.add(makeAction("←", () => moveSelected(Point(-1, 0))))
    tb.add(makeAction("→", () => moveSelected(Point(+1, 0))))
    tb.add(makeAction("↑", () => moveSelected(Point(0, -1))))
    tb.add(makeAction("↓", () => moveSelected(Point(0, +1))))

    // Tools
    val buttonGroup = new ButtonGroup()

    def addTool(text: String, newTool: Tool, mnemonic: Option[Char] = None): Unit = {
      val button = new JToggleButton(text)
      mnemonic foreach button.setMnemonic
      tb.add(button)
      buttonGroup.add(button)
      button.addActionListener(e => {
        tool.endDrag()
        tool = newTool
      })
    }

    addTool("Select", selectionTool, Some('S'))
    addTool("Move", moveTool, Some('e'))

    tb.add(makeAction("Mirror", () => foldSelectedIn()))

    tb.add(makeAction("Run SOMSolver", () => runSOMSolver()))
    tb.add(makeAction("Force solver", () => runForceSolver()))
    tb.add(makeAction("Random (full)", () => {
      solution = SOMSolver.randomInitialCoords(problem).toVector
      repaint()
      updateStatus()
    }))
    tb
  }

  private lazy val solutionIsValidText =
    new JTextField().tap(tf => tf.setColumns(5))

  private lazy val solutionDislikesText =
    new JTextField().tap(tf => tf.setColumns(5))

  private lazy val statusPanel = {
    val tb = new JToolBar()
    tb.add(new JLabel("Valid:"))
    tb.add(solutionIsValidText)
    tb.add(new JLabel("Dislikes count:"))
    tb.add(solutionDislikesText)
    tb
  }

  private def updateStatus(): Unit = {
    val sol = Solution(solution, null)
    val validator = new SolutionValidator(problem)
    val evaluator = new SolutionEvaluator(problem)

    val valid = validator.validate(sol)
    val dislikes = evaluator.evaluate(sol)

    solutionIsValidText.setText(valid.toString)
    solutionDislikesText.setText(dislikes.toString())

    problemPanel.repaint()
  }

  private lazy val somOptionsPanel = new SOMSolverOptionsPanel

  private def runSOMSolver(): Unit = {
    val options = somOptionsPanel.options
    val solver = new SOMSolver(problem, options)
    val init = solution
    val result = solver.optimize(init)
    result foreach { s =>
      solution = s.vertices
      repaint()
      updateStatus()
    }
  }

  private def runForceSolver(): Unit = {
    val result = ForceBasedSolver.stepForward(problem, Solution(solution, null))
    solution = result.vertices
    repaint()
    updateStatus()
  }

  private def moveSelected(delta: Point): Unit = {
    solution = solution.zipWithIndex.map { case (p, idx) =>
      if (selectionTool.selectedFigureVertices.contains(idx)) p + delta else p
    }
    updateStatus()
  }

  private def foldSelectedIn() : Unit = {
    if (selectionTool.selectedFigureVertices.size == 1) {
      for (index <- selectionTool.selectedFigureVertices) {
        val figure = Figure(problem.figure.edges, solution)
        DumbSolver.foldInOne(figure, index) match {
          case Some(newFigure) =>
            solution = newFigure.vertices
          case None => ()
        }
      }
      repaint()
      updateStatus()
    }
  }

  private def printSolution(): Unit = {
    val sol = Solution(solution, null)
    println(Json.serializeSolution(sol))
  }

  private def init(): Unit = {
    translator = new Translator(problem)
    solution = problem.figure.vertices
    originalEdgeLengths = problem.figure.edges.map(e => solution(e.vertex1) distanceSq solution(e.vertex2))

    setTitle(s"Visualizer - ${problemFile.getFileName.toString}")
    updateStatus()

    selectionTool.reset()
    moveTool.reset()
  }

  init()

  getContentPane.add(mainPanel)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setLocationByPlatform(true)
  pack()
}

object Visualizer {
  def show(problemFile: Path): Visualizer = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val v = new Visualizer(problemFile, problem)
    v.setVisible(true)
    v
  }
}
