package org.codingteam.icfpc2021.submitter

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

case class HelloResponse(hello: String)
case class SolutionResponse(state: String, dislikes: BigInt, error: String)
case class PostResponse(id: String)

object SubmitterJson {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def readHello(content: String): HelloResponse =
    mapper.readValue(content, classOf[HelloResponse])

  def readSolution(content: String): SolutionResponse =
    mapper.readValue(content, classOf[SolutionResponse])

  def readPost(content: String): PostResponse =
    mapper.readValue(content, classOf[PostResponse])
}
