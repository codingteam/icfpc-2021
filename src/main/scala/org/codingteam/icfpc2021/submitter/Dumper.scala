package org.codingteam.icfpc2021.submitter

import org.jsoup.Jsoup

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Dumper {
  val problemCount = 78
  def dump(sessionId: String, apiKey: String, directory: Path): Unit = {
    val client = Submitter.httpClient()
    for (problemId <- 1.to(problemCount)) {
      println(s"Working on problem $problemId.")
      println("  Downloading document.")
      val document = Jsoup.connect(s"https://poses.live/problems/$problemId")
        .cookie("session", sessionId)
        .get()
      val tbody = document.select("body > section > table > tbody")
      val rows = tbody.select("tr").asScala
      println(s"  ${rows.size - 1} solutions uploaded.")

      val data = rows.drop(1).view.map(tr => {
        val anchor = tr.select("td > a")
        val solutionId = anchor.attr("href").replace("/solutions/", "")
        DumperSolution(solutionId, Submitter.getProblemSolution(client, apiKey, problemId.toString, solutionId))
      }).toArray
      val results = DumperJson.serialize(data)
      val path = directory.resolve(s"$problemId.solutions.json")
      if (data.length > 0) {
        Files.writeString(path, results)
      }
      else
        Files.deleteIfExists(path)
    }
  }

  def analyze(solutionsDir: Path): Unit = {
    println(s"Analyzing data in directory $solutionsDir.")
    println()

    val files = Files.newDirectoryStream(solutionsDir)
    try {
      files.forEach(file => {
        if (file.getFileName.toString.contains(".solutions.")) {
          println(s"Analyzing file ${file.getFileName}.")
          val solutions = DumperJson.deserialize(Files.readString(file))
          val headSolution = solutions.head.solution
          val minDislikes = solutions.map(_.solution).filter(_.dislikes != null).map(_.dislikes).min
          if (headSolution.dislikes > minDislikes) {
            println(s"  Problem: minimal dislike count $minDislikes is less than the currently uploaded solution (${headSolution.dislikes}).")
          } else {
            println("  OK.")
          }
        }
      })
    } finally {
      files.close()
    }
  }
}
