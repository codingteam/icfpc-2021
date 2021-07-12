package org.codingteam.icfpc2021

import java.nio.file.{Files, Path}
import org.codingteam.icfpc2021.{Problem, Json}

/** Helpers to be used in the repl */
object Repl {
  def load(problem: Int): Problem = 
    Json.parseProblem(Files.readString(Path.of(s"problems/${problem}.json")))
}
