package org.codingteam.icfpc2021.genetic_solver

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.validator.SolutionValidator

import java.nio.file.{Files, Path}
import scala.util.Random
import scala.collection.immutable.TreeSet

class GeneticSolver(problem: Problem) {
  private val MaxIterations: Int = 100000
  private val GenerationSize: Int = 1000
  /// How many of creature's genes should change when mutating
  private val MutationPercentage: Double = 0.1
  /// How many of creature's genes should be replaced when crossing with another creature
  private val CrossoverPercentage: Double = 0.1

  /// If the best result didn't improve in the last `RestartAfter` iterations,
  /// take the `InheritCreatures` topmost creatures, stick them into a new
  /// initial generation, and continue the evolution from there.
  private val RestartAfter: Int = 3333
  private val InheritCreatures: Int = 100

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

  private def cross(a: Creature, b: Creature): Creature = {
    val genes_to_copy = 1.max((a.size.toDouble * CrossoverPercentage).toInt)
    val src_indices = Random.shuffle((0 until a.size).toVector).take(genes_to_copy)
    val dst_indices = Random.shuffle((0 until b.size).toVector).take(genes_to_copy)

    var mutable = b.toArray
    for ((src, dst) <- src_indices.zip(dst_indices)) {
      mutable(dst) = a(src)
    }
    mutable.toVector
  }

  private def evolve(): Option[Creature] = {
    val validator = new SolutionValidator(problem)

    var generation = produceInitialGeneration()

    // Last best score and the iteration at which it was attained.
    var last_best_score: (BigInt, Int) = (generation.head.score, 0)
    var iterations_without_improvement = 0

    for (i <- 0 until MaxIterations) {
      if (generation.head.score < last_best_score._1) {
        last_best_score = (generation.head.score, i)
        iterations_without_improvement = 0
      } else {
        iterations_without_improvement += 1
      }

      if (i % 100 == 0 || generation.head.score == 0) {
        val score = generation.head.score

        val solution = creatureToSolution(generation.head.creature)
        val valid = validator.validate(solution)

        println(f"[$i%7d]   Best score: $score%6d   Valid: $valid   No improvements for ${iterations_without_improvement}%4d iters")
      }

      if (generation.head.score == 0) {
        return Some(generation.head.creature)
      }

      if (iterations_without_improvement > RestartAfter) {
        println(f"[$i%7d]  RESTART")
        generation = (generation.take(InheritCreatures) ++ produceInitialGeneration()).take(GenerationSize)

        last_best_score = (generation.head.score, i)
        iterations_without_improvement = 0
      }

      import scala.collection.parallel.CollectionConverters._
      val newGeneration = generation.par.map { scored =>
        if (Random.nextBoolean()) {
          ScoredCreature(mutate(scored.creature))
        } else {
          val another_idx = Random.nextInt(generation.size)
          val another = generation.toIndexedSeq(another_idx)
          ScoredCreature(cross(scored.creature, another.creature))
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
