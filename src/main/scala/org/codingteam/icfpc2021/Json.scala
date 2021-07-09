package org.codingteam.icfpc2021

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, ScalaObjectMapper}

object Json {
  //noinspection ScalaDeprecation
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def parseProblem(content: String): Problem = {
    mapper.readValue[Problem](content, classOf[Problem])
  }
}
