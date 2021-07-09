package org.codingteam.icfpc2021.visualizer

import org.codingteam.icfpc2021.Json

import java.nio.file.{Files, Path}
import scala.swing.Frame

object Visualizer {
  def show(problemFile: Path): Unit = {
    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    val hole = problem.hole

    new Frame {
      title = "visualizer"

      pack()
      centerOnScreen()
      open()
    }
  }
}
