package org.codingteam.icfpc2021.genetic_solver

import org.codingteam.icfpc2021._
import org.codingteam.icfpc2021.validator.SolutionValidator

import java.nio.file.{Files, Path}
import scala.util.Random
import scala.collection.immutable.TreeSet
import scala.collection.concurrent.TrieMap

class GeneticSolver(problem: Problem) {
  private val MaxIterations: Int = 100000
  private val GenerationSize: Int = 1000
  /// How many of creature's genes should change when mutating
  private val MutationPercentage: Double = 0.1
  /// How many of creature's genes should be replaced when crossing with another creature
  private val CrossoverPercentage: Double = 0.1

  /// If the best result didn't improve in the last `RestartAfter` iterations,
  /// replace `BestCreaturesToDrop` topmost creatures with random ones and
  /// continue the evolution from there.
  private val RestartAfter: Int = 3333
  private val BestCreaturesToDrop: Int = 20

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

  private val validSegments: TrieMap[(Point, Point), Boolean] = TrieMap()
  private def segmentGoesOutsideTheHole(segment: (Point, Point)): Boolean = {
    var (p1, p2) = segment
    if (p1.x > p2.x) {
      val temp = p1
      p1 = p2
      p2 = temp
    } else if (p1.x == p2.x) {
      if (p1.y > p2.y) {
        val temp = p1
        p1 = p2
        p2 = temp
      }
    }
    val new_segment = (p1, p2)
    validSegments.getOrElseUpdate(new_segment, problem.segmentGoesOutsideTheHole(new_segment))
  }

  private def calculateScore(creature: Creature): Score = {
    val creatureVertices: Vector[Point] = creature.map(problem.hole(_))

    (for ((edge, expected_length) <- edges_sq_lengths) yield {
      val p1 = creatureVertices(edge.vertex1)
      val p2 = creatureVertices(edge.vertex2)

      if (segmentGoesOutsideTheHole((p1, p2))) {
        // Severely punish any solution where edges go outside the hole
        expected_length * expected_length
      } else {
        val actual_length = p1.distanceSq(p2)
        val diff = actual_length - expected_length

        /// WTH, there's no abs() for BigInt?
        if (diff < 0) {
          -diff
        } else {
          diff
        }
      }
    }).sum
  }

  private case class ScoredCreature(creature: Creature) {
    lazy val score: Score = calculateScore(creature)
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
    var last_best_score: (Score, Int) = (generation.head.score, 0)
    var iterations_without_improvement = 0

    var absolute_best: (Score, Creature) = (generation.head.score, generation.head.creature)

    for (i <- 0 until MaxIterations) {
      if (generation.head.score < last_best_score._1) {
        last_best_score = (generation.head.score, i)
        iterations_without_improvement = 0
      } else {
        iterations_without_improvement += 1
      }

      if (generation.head.score < absolute_best._1) {
        absolute_best = (generation.head.score, generation.head.creature)
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
        println(s"\tCurrent best: ${Json.serializeSolution(creatureToSolution(generation.head.creature))}")
        generation = (generation.drop(BestCreaturesToDrop) ++ produceInitialGeneration()).take(GenerationSize)

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


    if (absolute_best._1 == 0) {
      Some(absolute_best._2)
    } else {
      println(s"The best solution we ever got had a score of ${absolute_best._1}:")
      println(s"\t${Json.serializeSolution(creatureToSolution(absolute_best._2))}")

      None
    }
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
