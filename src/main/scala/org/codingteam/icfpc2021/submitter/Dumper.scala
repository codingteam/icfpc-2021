package org.codingteam.icfpc2021.submitter

import org.jsoup.Jsoup

import java.net.http.HttpClient
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Dumper {
  val problemCount = 132
  def dump(sessionId: String, apiKey: String, directory: Path): Unit = {
    val filesToRemove = ListBuffer[String]()
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
        dumpTopResult(client, sessionId, problemId, data, directory, filesToRemove)
      }
      else
        Files.deleteIfExists(path)
    }

    if (filesToRemove.nonEmpty) {
      println("Please remove the following outdated files:")
      for (file <- filesToRemove) {
        println(s"  $file")
      }
    }
  }

  def dumpTopResult(client: HttpClient, sessionId: String, problemId: Int, solutions: Array[DumperSolution], directory: Path, filesToRemove: ListBuffer[String]): Unit = {
    val result = solutions.head
    if (result.solution.dislikes == null) {
      println("  Top result is not valid, skipping.")
      val stream = Files.newDirectoryStream(directory, s"$problemId.json*")
      try {
        stream.forEach(path => {
          filesToRemove.addOne(path.toString)
        })
      } finally {
        stream.close()
      }
      return
    }

    var fileName = s"$problemId.json.${result.solution.dislikes}"
    if (result.solution.awardedBonuses.nonEmpty) {
      fileName += result.solution.awardedBonuses
        .sortBy(_.problem)
        .map(bonus => s"${bonus.bonus.charAt(0)}${bonus.problem}").mkString("-", "-", "")
    }

    println(s"  Downloading solution ${result.id}.")
    val solutionContent = Submitter.downloadSolution(client, sessionId, result.id)
    Files.writeString(directory.resolve(fileName), solutionContent)

    val stream = Files.newDirectoryStream(directory, s"$problemId.json*")
    try {
      stream.forEach(path => {
        if (path.getFileName.toString != fileName) {
          filesToRemove.addOne(path.toString)
        }
      })
    } finally {
      stream.close()
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
          val minDislikes = solutions.map(_.solution).filter(_.dislikes != null).map(_.dislikes).minOption
          if (minDislikes.isEmpty) {
            println("  OK (no valid solutions uploaded).")
          } else if (headSolution.dislikes == null || headSolution.dislikes > minDislikes.get) {
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
