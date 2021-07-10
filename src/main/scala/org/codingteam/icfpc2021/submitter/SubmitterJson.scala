package org.codingteam.icfpc2021.submitter

import com.fasterxml.jackson.databind.{ObjectMapper, PropertyNamingStrategies}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

case class HelloResponse(hello: String)
case class SolutionResponse(state: String, dislikes: BigInt, error: String, awardedBonuses: Vector[Nothing])
case class PostResponse(id: String)

object SubmitterJson {
  val mapper = new ObjectMapper()
  mapper.setPropertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE)
  mapper.registerModule(DefaultScalaModule)

  def readHello(content: String): HelloResponse =
    mapper.readValue(content, classOf[HelloResponse])

  def readSolution(content: String): SolutionResponse =
    mapper.readValue(content, classOf[SolutionResponse])

  def readPost(content: String): PostResponse =
    mapper.readValue(content, classOf[PostResponse])
}
