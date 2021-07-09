package org.codingteam.icfpc2021

import org.codingteam.icfpc2021.visualizer.Visualizer
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.rotation_solver.RotationSolver

import java.nio.file.Path

object Main extends App {
  args match {
    case Array("rotation-solver", path) => RotationSolver.solve(Path.of(path))
    case Array("validator", problemPath, solutionPath) => SolutionValidator.validateFile(Path.of(problemPath), Path.of(solutionPath))
    case Array("visualizer", path) => Visualizer.show(Path.of(path))
    case _ => println(
      """Possible arguments:
        |
        |rotation-solver <problem.json>
        |  Solve problem by rotating the figure.
        |
        |validator <problem.json> <solution.json>
        |  Check whether or not a given solution solves the given problem.
        |
        |visualizer <problem.json>
        |  Visualize a given problem.""".stripMargin)
  }
}
