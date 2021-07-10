package org.codingteam.icfpc2021.submitter

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.nio.file.{Files, Path}

object Submitter {
  private val apiAddress = "https://poses.live/"

  def submit(apiKey: String, path: Path): Unit = {
    checkApiKey(apiKey)
    processSolutionDirectory(apiKey, path)
  }

  def httpClient(): HttpClient = {
    HttpClient.newBuilder()
      .build()
  }

  private def httpRequest(apiKey: String): HttpRequest.Builder = {
    HttpRequest.newBuilder()
      .header("Authorization", s"Bearer $apiKey")
  }

  private def checkApiKey(apiKey: String): Unit = {
    val client = httpClient()
    val request = httpRequest(apiKey)
      .uri(URI.create(s"$apiAddress/api/hello"))
      .build()
    val response = SubmitterJson.readHello(client.send(request, BodyHandlers.ofString()).body())
    if (response.hello != "codingteam") throw new Exception(s"Invalid API check response: $response")
  }

  def getProblemSolution(client: HttpClient, apiKey: String, problemId: String, solutionId: String): SolutionResponse = {
    val request = httpRequest(apiKey)
      .uri(URI.create(s"$apiAddress/api/problems/$problemId/solutions/$solutionId"))
      .build()
    SubmitterJson.readSolution(client.send(request, BodyHandlers.ofString()).body())
  }

  private def processSolutionDirectory(apiKey: String, path: Path): Unit = {
    val files = Files.newDirectoryStream(path)
    try {
      println(s"Processing directory $path")
      val client = httpClient()

      files.forEach(file => {
        if (file.toString.endsWith(".json")) {
          println(s"Processing file ${file.getFileName}")
          val problemId = file.getFileName.toString.replaceFirst("\\.[^.]$", "")
          println(s"  Processing problem $problemId")
//          val status = getProblemSolution(client, apiKey, problemId)
//          println(s"  Current problem status: $status")
        }
      })
    } finally {
      files.close()
    }
  }
}
