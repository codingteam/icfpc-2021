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
        Files.delete(path)
    }
  }
}
