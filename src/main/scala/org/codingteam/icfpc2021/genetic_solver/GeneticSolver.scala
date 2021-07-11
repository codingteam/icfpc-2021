package org.codingteam.icfpc2021.genetic_solver

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.evaluator.SolutionEvaluator

import java.nio.file.{Files, Path}
import scala.util.Random
import scala.collection.immutable.TreeSet

class GeneticSolver(problem: Problem) {
  private val MaxIterations: Int = 100000;
  private val GenerationSize: Int = 1000;
  /// How many of creature's genes should change when mutating.
  private val MutationPercentage: Double = 0.1;

  /// Squared lengths of edges.
  private val edges_sq_lengths: Vector[(Edge, BigInt)] = {
    (for (edge <- problem.figure.edges) yield {
      val p1 = problem.figure.vertices(edge.vertex1)
      val p2 = problem.figure.vertices(edge.vertex2)
      (edge, p1.distanceSq(p2))
    }).toVector
  }

  /// A creature is a vector of indices into the problem's `hole`.
  private type Creature = Vector[Int]

  /// Creature's score indicates how much its edges differ from the edges of
  /// the problem's figure. The score of 0 means this creature is a potentional
  /// solution to the problem.
  private type Score = BigInt

  private case class ScoredCreature(creature: Creature) {
    lazy val score: BigInt = {
      val creatureVertices: Vector[Point] = creature.map(problem.hole(_))

      (for ((edge, expected_length) <- edges_sq_lengths) yield {
        val p1 = creatureVertices(edge.vertex1)
        val p2 = creatureVertices(edge.vertex2)
        val actual_length = p1.distanceSq(p2)
        val diff = actual_length - expected_length

        /// WTH, there's no abs() for BigInt?
        if (diff < 0) {
          -diff
        } else {
          diff
        }
      }).sum
    }
  }
  private object ScoredCreature {
    implicit def orderingScoredCreature(implicit ord: Ordering[Score]): Ordering[ScoredCreature] =
      Ordering.by(_.score)
  }

  private type Generation = TreeSet[ScoredCreature]

  private def produceInitialGeneration(): Generation = {
    val attempts = GenerationSize * 10;
    val creatures = (0 until attempts).map(_ => {
      val creature = (0 until problem.figure.vertices.size).map(_ => Random.nextInt(problem.hole.size)).toVector
      ScoredCreature(creature)
    })

    TreeSet.from(creatures).take(GenerationSize)
  }

  private def mutate(creature: Creature): Creature = {
    val genes_to_mutate = 1.max((creature.size.toDouble * MutationPercentage).toInt)
    val indices = Random.shuffle((0 until creature.size).toVector).take(genes_to_mutate)

    var mutable = creature.toArray
    for (i <- indices) {
      mutable(i) = Random.nextInt(problem.hole.size)
    }
    mutable.toVector
  }

  private def evolve(): Option[Creature] = {
    val validator = new SolutionValidator(problem)
    val evaluator = new SolutionEvaluator(problem)

    var generation = produceInitialGeneration()

    for (i <- 0 until MaxIterations) {
      if (i % 100 == 0 || generation.head.score == 0) {
        val score = generation.head.score

        val solution = creatureToSolution(generation.head.creature)
        val valid = validator.validate(solution)
        val dislikes: BigInt = if (valid) {
          evaluator.evaluate(solution)
        } else {
          -1
        }

        println(f"[$i%7d]  Best score: $score%6d  Dislikes: $dislikes%6d  Valid: $valid")
      }

      if (generation.head.score == 0) {
        return Some(generation.head.creature)
      }

      import scala.collection.parallel.CollectionConverters._
      val newGeneration = generation.par.map { scored =>
        if (Random.nextBoolean()) {
          ScoredCreature(mutate(scored.creature))
        } else {
          // crossover
          scored
        }
      }

      generation = (generation ++ newGeneration).take(GenerationSize)
    }

    None
  }

  private def creatureToSolution(creature: Creature): Solution = {
    val vertices = creature.map(i => problem.hole(i))
    val bonuses = Vector()
    Solution(vertices, bonuses)
  }

  def solve(): Option[Solution] = {
    evolve().map(creatureToSolution)
  }
}

object GeneticSolverCli {
  def process(file: Path): Unit = {
    val problem = Json.parseProblem(Files.readString(file))
    val solution = new GeneticSolver(problem).solve().map(Json.serializeSolution(_))
    println(solution)
  }
}
