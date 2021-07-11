package org.codingteam.icfpc2021.sat_solver

import org.codingteam.icfpc2021._

import java.nio.file.{Files, Path}

class SatSolver(problem: Problem) {
  def solve(): Option[Solution] = {
    val logic = new BooleanLogic()
    println(s"${logic.toCNF()}")
    None
  }
}

object SatSolverCli {
  def solve(path: Path): Unit = {
    val problem = Json.parseProblem(Files.readString(path))
    val solution = new SatSolver(problem).solve().map(Json.serializeSolution(_))
    println(solution)
  }
}
