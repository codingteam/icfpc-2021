package org.codingteam.icfpc2021.submitter

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

case class DumperSolution(id: String, solution: SolutionResponse)

object DumperJson {
  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def serialize(array: Seq[DumperSolution]): String = {
    mapper.writerWithDefaultPrettyPrinter().writeValueAsString(array)
  }

  def deserialize(content: String): Seq[DumperSolution] = {
    mapper.readValue[Seq[DumperSolution]](content, new TypeReference[Seq[DumperSolution]] {})
  }
}
