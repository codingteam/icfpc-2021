package org.codingteam.icfpc2021

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.nio.file.{Files, Path}
import scala.swing.Frame

case class Person( name: String, age: Int )

object Visualizer {
  def show(problemFile: Path): Unit = {

    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    mapper.readValue[Person]("{ \"name\": \"Vas\", \"age\": 123 }", classOf[Person])


    val content = Files.readString(problemFile)
    val problem = Json.parseProblem(content)

    new Frame {
      title = "visualizer"

      pack()
      centerOnScreen()
      open()
    }
  }
}
