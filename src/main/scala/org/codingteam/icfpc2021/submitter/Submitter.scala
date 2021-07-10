package org.codingteam.icfpc2021.submitter

import org.codingteam.icfpc2021.evaluator.SolutionEvaluator
import org.codingteam.icfpc2021.validator.SolutionValidator
import org.codingteam.icfpc2021.{Json, Solution}

import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
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

  def dislikesOfLastSolution(solutionsDir: Path, problemId: String): BigInt = {
    val problemJsonFile = solutionsDir.resolve(s"$problemId.solutions.json")
    if (!Files.exists(problemJsonFile)) {
      println("  No solutions file, no existing solution known.")
      return null
    }

    val content = Files.readString(problemJsonFile)
    val lastSolution = DumperJson.deserialize(content).head.solution
    if (lastSolution.error != null) {
      println("  Last known solution errored.")
      return null
    }

    lastSolution.dislikes
  }

  private def uploadSolution(solutionsDir: Path, client: HttpClient, apiKey: String, problemId: String, solution: Solution, dislikes: BigInt): Unit = {
    println("  Sending solution to server.")

    val request = httpRequest(apiKey)
      .uri(URI.create(s"$apiAddress/api/problems/$problemId/solutions"))
      .POST(BodyPublishers.ofString(Json.serializeSolution(solution)))
      .build()

    val response = client.send(request, BodyHandlers.ofString())
    println(s"  Result: ${response.statusCode()}.")

    val responseBody = response.body()
    println(s"  Response body: $responseBody.")

    val solutionId = SubmitterJson.readPost(responseBody).id
    println(s"  Solution id: $solutionId.")

    if (response.statusCode() == 200) {
      val solutionsFile = solutionsDir.resolve(s"$problemId.solutions.json")
      val solutions = DumperJson.deserialize(Files.readString(solutionsFile))
      val newSolutions = DumperSolution(solutionId, SolutionResponse("JUST_SENT", dislikes, null)) +: solutions
      Files.writeString(solutionsFile, DumperJson.serialize(newSolutions))
    }
  }

  private def processSolutionDirectory(apiKey: String, solutionsDir: Path): Unit = {
    val files = Files.newDirectoryStream(solutionsDir)
    try {
      val problemsDir = solutionsDir.resolve("../problems")
      println(s"Processing directory $solutionsDir")
      println(s"Problems dir: $problemsDir")
      println()

      val client = httpClient()

      files.forEach(file => {
        try {
          val fileName = file.getFileName
          if (fileName.toString.contains(".json") && !fileName.toString.contains("solutions")) {
            println(s"Processing file $fileName")
            val problemId = fileName.toString.replaceFirst("\\.json.*$", "")
            println(s"  Processing problem $problemId.")

            val problem = Json.parseProblem(Files.readString(problemsDir.resolve(s"$problemId.json")))
            val solution = Json.parseSolution(Files.readString(file))

            if (new SolutionValidator(problem).validate(solution)) {
              println("  Local solution valid.")
              val localDislikes = new SolutionEvaluator(problem).evaluate(solution)
              val serverDislikes = dislikesOfLastSolution(solutionsDir, problemId)
              println(s"  Local dislikes: $localDislikes, server dislikes: $serverDislikes.")
              if (serverDislikes == null || localDislikes < serverDislikes) {
                uploadSolution(solutionsDir, client, apiKey, problemId, solution, localDislikes)
              }
            } else {
              println("  Local solution invalid.")
            }
          }
        } catch {
          case e: Throwable =>
            System.err.println(e)
            e.printStackTrace(System.err)
        }
      })
    } finally {
      files.close()
    }
  }
}
