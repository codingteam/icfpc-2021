package org.codingteam.icfpc2021

import org.codingteam.icfpc2021.rotation_solver.RotationSolver
import org.codingteam.icfpc2021.submitter.{Dumper, Submitter}
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.visualizer.Visualizer

import java.nio.file.Path

object Main extends App {
  args match {
    case Array("rotation-solver", path) => RotationSolver.solve(Path.of(path))
    case Array("validator", problemPath, solutionPath) => SolutionValidator.validateFile(Path.of(problemPath), Path.of(solutionPath))
    case Array("visualizer", path) => Visualizer.show(Path.of(path))
    case Array("submitter", apiKey, path) => Submitter.submit(apiKey, Path.of(path))
    case Array("submitter", apiKey) => Submitter.submit(apiKey, Path.of("solutions"))
    case Array("dumper", sessionId, apiKey, directory) => Dumper.dump(sessionId, apiKey, Path.of(directory))
    case Array("dumper", sessionId, apiKey) => Dumper.dump(sessionId, apiKey, Path.of("solutions"))
    case Array("dumper-analyzer", directory) => Dumper.analyze(Path.of(directory))
    case Array("dumper-analyzer") => Dumper.analyze(Path.of("solutions"))
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
        |  Visualize a given problem.
        |
        |submitter <api-key> [solutions-directory]
        |  Upload solutions from the solutions-directory to the server if they are better.
        |  Default value of solutions-directory is "solutions".
        |
        |dumper <session-id> <api-key> [solutions-directory]
        |  Dump current solution data to the solutions-directory. Requires session from browser cookies.
        |  Default value of solutions-directory is "solutions".
        |
        |dumper-analyzer [solutions-directory]
        |  Will analyze the data from dumper in the solutions-directory.
        |  Default value of solutions-directory is "solutions".
        |""".stripMargin)
  }
}
