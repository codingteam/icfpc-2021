package org.codingteam.icfpc2021.sat_solver

import org.codingteam.icfpc2021._

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable.{Buffer, HashMap}
import scala.math.abs

class SatSolver(problem: Problem) {
  /// Index of hole's index -> list of reachable indices along with squared distances to them
  private val validEdgesFrom: Vector[Vector[(Int, BigInt)]] = {
    val result: Array[Vector[(Int, BigInt)]] = new Array(problem.hole.size)
    for (from <- problem.hole.indices) yield {
      val reachable: Buffer[(Int, BigInt)] = Buffer()
      for (i <- problem.hole.indices if i != from) {
        val segment = (problem.hole(from), problem.hole(i))
        if (!problem.segmentGoesOutsideTheHole(segment)) {
          reachable += ((i, segment._1.distanceSq(segment._2)))
        }
      }
      result(from) = reachable.toArray.sortBy(_._2).toVector
    }
    result.toVector
  }

  /// Indices of edges that originate in a given figure vertex, along with the edge's length.
  private val figureEdgesFrom: Vector[Vector[(Int, BigInt)]] = {
    val result: Array[Buffer[(Int, BigInt)]] = new Array(problem.figure.edges.size)
    for (i <- result.indices) {
      result(i) = Buffer()
    }
    for (edge_index <- problem.figure.edges.indices) {
      val Edge(i, j) = problem.figure.edges(edge_index)
      val length = problem.figure.vertices(i).distanceSq(problem.figure.vertices(j))
      result(i) += ((j, length))
      result(j) += ((i, length))
    }
    result.map(_.toArray.sortBy(_._2).toVector).toVector
  }

  private def equalByEpsilon(expected: BigInt, actual: BigInt): Boolean = {
    val gcd = actual.gcd(expected)
    abs((actual/gcd).toDouble / (expected/gcd).toDouble - 1) <= (problem.epsilon.toDouble / 1e6)
  }

  // For each of the figure's vertices, we get a vector of hole's vertex
  // indexes at which this figure vertex can be placed
  private val suitableHoleVertices: Vector[Vector[Int]] = {
    (for (f <- problem.figure.vertices.indices) yield {
      val result: Buffer[Int] = Buffer()
      for (h <- problem.hole.indices) {
        // Does hole `h` have *all* the edges that the figure `f` requires?
        val required = figureEdgesFrom(f)
        val available = validEdgesFrom(h)
        val has_it_all =
          (for (r <- required) yield {
            available.find(e => equalByEpsilon(e._2, r._2)).isDefined
          }).fold(true)(_ && _)
        if (has_it_all) {
          result += h
        }
      }

      require(!result.isEmpty)
      result.toVector
    }).toVector
  }

  private def prepareDIMACS(): String = {
    val logic = BooleanLogic()

    // Each of the figure's vertices should correspond to exactly one of the
    // hole's vertices. (The inverse doesn't hold: multiple f-vertices can
    // share a single h-vertex.)
    for (f <- problem.figure.vertices.indices) {
      val terms = problem.hole.indices.map(h => (h, Term(1 + f*problem.hole.size + h)))
      val clause =
        (for (h <- problem.hole.indices) yield {
          terms
            .map(t => if (t._1 == h) { t._2 } else { Not(t._2) })
            .fold(AlwaysTrue)(And(_, _))
        }).fold(AlwaysFalse)(Or(_, _))
      logic.and(clause)
    }

    // For each f-vertex there are one or more "suitable" h-vertices, i.e.
    // vertices which can potentially form edges of the necessary length.
    //
    // These rules ensure that each f-vertex only occupies *one* of its
    // suitable h-vertices.
    for (f <- problem.figure.vertices.indices) {
      val suitable = suitableHoleVertices(f)
      val terms = suitable.map(h => Term(1 + f*problem.hole.size + h))
      val clause = terms.fold(AlwaysFalse)(Or(_, _))
      logic.and(clause)
    }

    // If an f-vertex occupied an h-vertex, make sure that all the h-vertices
    // it would want to connect to are also occupied.
    for (f <- problem.figure.vertices.indices) {
      val suitable = suitableHoleVertices(f)
      for (s <- suitable) {
        val available = validEdgesFrom(s)
        val require_counter_vertices =
          (for (required <- figureEdgesFrom(f)) yield {
            val wanted = available.filter(e => equalByEpsilon(e._2, required._2)).map(_._1)
            if (!wanted.isEmpty) {
              // Require that the hole h is used by *someone*
              val terms =
                wanted.flatMap(h => problem.figure.vertices.indices.map(f => Term(1 + f*problem.hole.size + h)))
              terms.fold(AlwaysFalse)(Or(_, _))
            } else {
              AlwaysTrue
            }
          }).fold(AlwaysTrue)(And(_, _))

        val f_s_term = Term(1 + f*problem.hole.size + s)
        // if f occupies s, then require counter vertices
        val clause = Or(Not(f_s_term), require_counter_vertices)
        logic.and(clause)
      }
    }

    DIMACS.from(logic)
  }

  private def runMinisat(dimacs: String): Option[String] = {
    import scala.sys.process._

    val inputFile: Path = Files.createTempFile("icfpc2021-satsolver-in-", ".sat")
    val outputFile: Path = Files.createTempFile("icfpc2021-satsolver-out-", ".sat")

    try {
      Files.writeString(inputFile, dimacs, StandardOpenOption.CREATE)
      val exit_code = Seq("minisat", inputFile.toString, outputFile.toString).!
      if (exit_code == 10) {
        // the problem was satisfied
        return Some(Files.readString(outputFile))
      }
    } finally {
      Files.delete(inputFile)
      Files.delete(outputFile)
    }

    None
  }

  def solve(): Option[Solution] = {
    val dimacs = prepareDIMACS()
    val sat_solution = runMinisat(dimacs)
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
