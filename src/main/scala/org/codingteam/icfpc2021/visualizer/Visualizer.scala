package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.force_solver.ForceBasedSolver
import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.solver.{DumbSolver, SolutionOptimizer, SolutionOptimizerPanel}
import org.codingteam.icfpc2021.som.{SOMSolver, SOMSolverOptionsPanel}
import org.codingteam.icfpc2021.validator.{EdgeCheckResult, SolutionValidator}

import java.awt.event.{
  KeyEvent,
  KeyListener,
  MouseEvent,
  MouseListener,
  MouseMotionListener,
  MouseWheelEvent,
  MouseWheelListener,
}
import java.awt.{BorderLayout, Color, Dimension, Graphics}
import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{BorderLayout, Color, Dimension, Graphics, GridLayout}
import java.nio.file.{Files, Path, Paths}
import java.io.File
import javax.swing._
import scala.collection.mutable
import scala.language.implicitConversions
import scala.swing.Graphics2D
import scala.util.Random
import scala.util.chaining._

class Visualizer(var problemFile: Path, var problem: Problem) extends JFrame("Codingteam ICPFC-2021") {

  private var translator: Translator = _
  private var solution: Vector[Point] = _
  private var originalEdgeLengths: Vector[BigInt] = _

  case class DragInfo(
    start: java.awt.Point,
    curr: java.awt.Point,
    prev: java.awt.Point,
  )

  class ToolHandler extends MouseListener with MouseMotionListener {
    var info: Option[DragInfo] = None
    private var tool = new Tool() {}

    def setTool(newTool: Tool): Unit = {
      info.foreach { info => tool.endDrag(info) }
      tool = newTool
    }

    override def mouseClicked(me: MouseEvent): Unit = {
      println(translator.toModel(me.getX, me.getY))
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {
      problemPanel.requestFocus()
      val newInfo = new DragInfo(
        new java.awt.Point(e.getX, e.getY),
        new java.awt.Point(e.getX, e.getY),
        new java.awt.Point(e.getX, e.getY),
      )
      info = Some(newInfo)
      tool.startDrag(newInfo)
      updateStatus()
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      info foreach { info =>
        tool.endDrag(info)
        updateStatus()
      }
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      info foreach { info =>
        info.curr.setLocation(e.getX, e.getY)
        tool.dragged(info)
        info.prev.setLocation(e.getX, e.getY)
        updateStatus()
      }
    }

    override def mouseMoved(e: MouseEvent): Unit = {
      val coords = translator.toModel(e.getX, e.getY)
      coordinatesText.setText(s"${coords.x}, ${coords.y}")

      tool.mouseMoved(e.getX, e.getY)
      updateStatus()
    }
  }
  val toolHandler = new ToolHandler()

  trait Tool {
    def startDrag(info: DragInfo): Unit = {}
    def endDrag(info: DragInfo): Unit = {}
    def dragged(info: DragInfo): Unit = {}
    def mouseMoved(x: Int, y: Int): Unit = {}
    def reset(): Unit = {}
  }

  class SelectionTool extends Tool {
    var selectedFigureVertices: mutable.BitSet = mutable.BitSet()
    var rect: Option[(java.awt.Point, java.awt.Point)] = None

    override def dragged(info: DragInfo): Unit = {
      val (x1, x2) = (info.start.x.min(info.curr.x), info.start.x.max(info.curr.x))
      val (y1, y2) = (info.start.y.min(info.curr.y), info.start.y.max(info.curr.y))
      this.rect = Some((new java.awt.Point(x1, y1), new java.awt.Point(x2, y2)))
      val rect = Rect(translator.toModel(x1, y1), translator.toModel(x2, y2))

      selectedFigureVertices.clear()
      for ((vert, i) <- solution.zipWithIndex) {
        if (rect.contains(vert)) {
          selectedFigureVertices.add(i)
        }
      }
    }

    override def endDrag(info: DragInfo): Unit = rect = None

    override def reset(): Unit = selectedFigureVertices = mutable.BitSet()
  }

  class MoveTool extends Tool {
    var prev: Point = Point(0, 0)

    override def startDrag(info: DragInfo): Unit = prev = Point(0, 0)

    override def dragged(info: DragInfo): Unit = {
      val curr = translator.toModelDelta(info.curr.x - info.start.x, info.curr.y - info.start.y)
      if (info.curr != info.prev) {
        moveSelected(curr - prev)
      }
      prev = curr
    }
  }

  class RotationTool extends Tool {
    // We store initial solution to avoid rounding error accumulation
    private var initialSolution: Option[Vector[Point]] = None

    override def startDrag(info: DragInfo): Unit = {
      initialSolution = Some(solution)
    }

    override def endDrag(info: DragInfo): Unit = {
      initialSolution = None
    }

    override def dragged(info: DragInfo): Unit = {
      initialSolution foreach { bs =>
        val angle = (info.start.x - info.curr.x) / 100.0
        solution = RotationSolver.rotate_by(angle, bs)
      }
    }
  }

  class DragTool extends Tool {
    private var prev: Point = Point(0, 0)
    var hover: Option[Int] = None
    private var move = false

    override def mouseMoved(x: Int, y: Int): Unit = {
      def distSq(xy: (Int, Int)): Int = (xy._1-x)*(xy._1-x) + (xy._2-y)*(xy._2-y)
      val (vert, i) = solution.zipWithIndex.minBy { case (vert, i) => distSq(translator.toScreen(vert)) }

      if (distSq(translator.toScreen(vert)) <= 16 * 16) {
        hover = Some(i)
      } else {
        hover = None
      }
    }

    override def dragged(info: DragInfo): Unit = {
      hover foreach { hover =>
        val curr = translator.toModelDelta(info.curr.x - info.start.x, info.curr.y - info.start.y)
        selectionTool.selectedFigureVertices.clear()
        selectionTool.selectedFigureVertices.add(hover)
        if (info.curr != info.prev) {
          moveSelected(curr - prev)
        }
        prev = curr
      }
    }

    override def startDrag(info: DragInfo): Unit = prev = Point(0, 0)
  }

  private val selectionTool = new SelectionTool
  private val moveTool = new MoveTool
  private val rotationTool = new RotationTool
  private val dragTool = new DragTool

  private var guidesMode = "no"

  private lazy val rightPanel = {
    val p = new JPanel()
    p.setLayout(new GridLayout(-1, 1))
    p.add(optimizerOptionsPanel)
    p.add(somOptionsPanel)
    p
  }

  private lazy val mainPanel = {
    val p = new JPanel()
    p.setLayout(new BorderLayout())
    p.add(problemPanel, BorderLayout.CENTER)
    p.add(buttonsPanel, BorderLayout.NORTH)
    p.add(statusPanel, BorderLayout.SOUTH)
    p.add(rightPanel, BorderLayout.EAST)
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

        dragTool.hover foreach { hover =>
          g2.setColor(Color.RED)
          val (x, y) = translator.toScreen(solution(hover))
          g.drawOval(x - 5, y - 5, 10, 10)
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

        def drawGuide(from: Int, to: Int): Unit = {
          val pFrom = translator.toScreen(solution(from))
          val pTo = translator.toScreen(solution(to))
          val range = problem.edgeDistRangeSqUnits(from, to)
          drawArc(g2, pFrom, pTo,
            translator.sqUnitsToScreen(range._1).toInt,
            translator.sqUnitsToScreen(range._2).toInt)
        }

        guidesMode match {
          case "no" => {}
          case "sel" =>
            for (i <- selectionTool.selectedFigureVertices)
              for (j <- problem.figure.vertexNeighbours(i))
                drawGuide(i, j)
          case "adj" =>
            for (i <- selectionTool.selectedFigureVertices)
              for (j <- problem.figure.vertexNeighbours(i))
                drawGuide(j, i)
          case "all" =>
            for (i <- problem.figure.vertices.indices)
              for (j <- problem.figure.vertexNeighbours(i))
                drawGuide(i, j)
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
    p.setPreferredSize(new Dimension(1730, 960))

    p.addMouseListener(toolHandler)
    p.addMouseMotionListener(toolHandler)

    p.addMouseWheelListener(new MouseWheelListener() {
       override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
         // TODO: zoom
         updateStatus()
       }
    })

    p.addKeyListener(new KeyListener() {
      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match {
          case KeyEvent.VK_LEFT => moveSelected(Point(-1, 0))
          case KeyEvent.VK_RIGHT => moveSelected(Point(+1, 0))
          case KeyEvent.VK_UP => moveSelected(Point(0, -1))
          case KeyEvent.VK_DOWN => moveSelected(Point(0, +1))
          case _ => {}
        }
      }
      override def keyReleased(e: KeyEvent): Unit = {}
    })

    p.setFocusable(true)

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

    tb.add(makeAction("Load solution", () => loadSolution()))

    tb.add(makeAction("Print JSON", () => printSolution()))

    // Tools
    val buttonGroup = new ButtonGroup()

    def addTool(text: String, newTool: Tool, mnemonic: Option[Char] = None): Unit = {
      val button = new JToggleButton(text)
      mnemonic foreach button.setMnemonic
      tb.add(button)
      buttonGroup.add(button)
      button.addActionListener(e => toolHandler.setTool(newTool))
    }

    tb.add({
      val cb = new JComboBox(Array("no", "sel", "adj", "all"))
      cb.addActionListener(e => {
        guidesMode = cb.getSelectedItem().asInstanceOf[String]
        repaint()
      })
    cb
    })

    addTool("Select", selectionTool, Some('S'))
    addTool("Move", moveTool, Some('e'))
    addTool("Rotate", rotationTool, Some('R'))
    addTool("Drag", dragTool, Some('D'))

    tb.add(makeAction("Mirror X", () => mirrorX()))
    tb.add(makeAction("Mirror Y", () => mirrorY()))
    tb.add(makeAction("X<->Y", () => transposeXY()))

    tb.add(makeAction("Mirror Vert", () => foldSelectedIn()))
    tb.add(makeAction("Wobble", () => wobbleSelected()))

    tb.add(makeAction("Run SOMSolver", () => runSOMSolver()))
    tb.add(makeAction("Force solver", () => runForceSolver()))
    tb.add(makeAction("Optimize", () => runOptimizer()))
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

  private lazy val coordinatesText =
    new JTextField().tap(tf => tf.setColumns(5))

  private lazy val statusPanel = {
    val tb = new JToolBar()
    tb.add(new JLabel("Valid:"))
    tb.add(solutionIsValidText)
    tb.add(new JLabel("Dislikes count:"))
    tb.add(solutionDislikesText)
    tb.add(new JLabel("Coordinates"))
    tb.add(coordinatesText)
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
  private lazy val optimizerOptionsPanel = new SolutionOptimizerPanel

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
    val result = ForceBasedSolver.stepForward(problem, Solution(solution, null), steps=100)
    solution = result.vertices
    repaint()
    updateStatus()
  }

  private def runOptimizer(): Unit = {
    val optimizer = new SolutionOptimizer(problem)
    val options = optimizerOptionsPanel.options
    solution = optimizer.optimizeOnce(solution, options)
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

  private def wobbleSelected(): Unit = {
    for (index <- selectionTool.selectedFigureVertices) {
      val res = DumbSolver.wobbleOne(problem, solution, index, delta=20)
      if (res.length >= 1) {
        val random = new Random()
        val i = if (res.length > 1)  {random.nextInt(res.length-1)} else 0
        println("Move")
        solution = res(i)
      }
    }
    repaint()
    updateStatus()
  }

  private def mirrorX() : Unit = {
    solution = DumbSolver.mirrorX(solution)
    repaint()
    updateStatus()
  }

  private def mirrorY() : Unit = {
    solution = DumbSolver.mirrorY(solution)
    repaint()
    updateStatus()
  }

  private def transposeXY() : Unit = {
    solution = DumbSolver.transposeXY(solution)
    repaint()
    updateStatus()
  }

  private def loadSolution(): Unit = {
    val cwd = System.getProperty("user.dir") + File.separator + "solutions"

    val dialog = new JFileChooser
    dialog.setCurrentDirectory(new File(cwd))
    val option = dialog.showOpenDialog(this)
    if (option == JFileChooser.APPROVE_OPTION) {
      val path = cwd + File.separator + dialog.getSelectedFile.getName
      val loadedSolution = Json.parseSolution(Files.readString(Paths.get(path)))
      solution = loadedSolution.vertices
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
