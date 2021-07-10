package org.codingteam.icfpc2021.triangles

import org.codingteam.icfpc2021._

import java.nio.file.{Files, Path}

object FindTriangles {
  def process(file: Path): Unit = {
    val problem = Json.parseProblem(Files.readString(file))
    println(problem.figure.triangleGroups)
  }
}
