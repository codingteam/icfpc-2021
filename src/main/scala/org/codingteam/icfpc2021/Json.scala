package org.codingteam.icfpc2021

import com.fasterxml.jackson.core.{JsonParseException, JsonParser, JsonToken}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.{DeserializationContext, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object Json {
  private val icfpc2021JsonModule = new SimpleModule("Icfpc2021JsonModule")
  icfpc2021JsonModule.addDeserializer(
    classOf[Point],
    (p: JsonParser, _: DeserializationContext) => {
      if (p.nextToken() != JsonToken.VALUE_NUMBER_INT)
        throw new JsonParseException(p, "Invalid start of X coordinate")
      val x = BigInt(p.getBigIntegerValue())
      if (p.nextToken() != JsonToken.VALUE_NUMBER_INT)
        throw new JsonParseException(p, "Invalid start of Y coordinate")
      val y = BigInt(p.getBigIntegerValue())
      if (p.nextToken() != JsonToken.END_ARRAY)
        throw new JsonParseException(p, "Invalid end for Point")
      Point(x, y)
    },
  )
  icfpc2021JsonModule.addDeserializer(
    classOf[Edge],
    (p: JsonParser, _: DeserializationContext) => {
      val x = p.nextIntValue(0)
      val y = p.nextIntValue(0)
      if (p.nextToken() != JsonToken.END_ARRAY)
        throw new JsonParseException(p, "Invalid end for Point")
      Edge(x, y)
    },
  )

  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(icfpc2021JsonModule)

  def parseProblem(content: String): Problem = {
    mapper.readValue[Problem](content, classOf[Problem])
  }

  def parseSolution(content: String): Solution = {
    mapper.readValue[Solution](content, classOf[Solution])
  }
}
