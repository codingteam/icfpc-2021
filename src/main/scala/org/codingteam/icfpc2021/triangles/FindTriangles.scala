package org.codingteam.icfpc2021.triangles

import org.codingteam.icfpc2021._

import java.nio.file.{Files, Path}
import collection.mutable.Map

class Triangles(figure: Figure) {
  lazy val triangles: Vector[(Int, Int, Int)] = {
    var connections: Map[Int, Set[Int]] = Map()
    for (edge <- figure.edges) {
      val target1 = connections.getOrElseUpdate(edge.vertex1, Set())
      connections.update(edge.vertex1, target1 + edge.vertex2)

      val target2 = connections.getOrElseUpdate(edge.vertex2, Set())
      connections.update(edge.vertex2, target2 + edge.vertex1)
    }

    figure.vertices.indices
      .flatMap(idx =>
        connections(idx)
          .filter(_ > idx)
          .flatMap(snd_idx =>
            connections(idx)
              .intersect(connections(snd_idx))
              .filter(_ > snd_idx)
              .map((idx, snd_idx, _)),
          ),
      )
      .toVector
  }
}

object FindTriangles {
  def process(file: Path): Unit = {
    val problem = Json.parseProblem(Files.readString(file))
    val triangles = new Triangles(problem.figure)
    println(triangles.triangles)
  }
}
