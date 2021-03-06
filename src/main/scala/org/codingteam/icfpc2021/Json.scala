package org.codingteam.icfpc2021

import com.fasterxml.jackson.core.{JsonGenerator, JsonParseException, JsonParser, JsonToken}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.{DeserializationContext, ObjectMapper, PropertyNamingStrategies, SerializerProvider}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object Json {
  private val icfpc2021JsonModule = new SimpleModule("Icfpc2021JsonModule")

  icfpc2021JsonModule.addSerializer(classOf[Point], (value: Point, gen: JsonGenerator, _: SerializerProvider) => {
    gen.writeStartArray(value)
    gen.writeNumber(value.x.bigInteger)
    gen.writeNumber(value.y.bigInteger)
    gen.writeEndArray()
  })
  icfpc2021JsonModule.addDeserializer(classOf[Point], (p: JsonParser, _: DeserializationContext) => {
    if (p.nextToken() != JsonToken.VALUE_NUMBER_INT)
      throw new JsonParseException(p, "Invalid start of X coordinate")
    val x = BigInt(p.getBigIntegerValue)
    if (p.nextToken() != JsonToken.VALUE_NUMBER_INT)
      throw new JsonParseException(p, "Invalid start of Y coordinate")
    val y = BigInt(p.getBigIntegerValue)
    if (p.nextToken() != JsonToken.END_ARRAY)
      throw new JsonParseException(p, "Invalid end for Point")
    Point(x, y)
  })

  icfpc2021JsonModule.addSerializer(classOf[Edge], (value: Edge, gen: JsonGenerator, _: SerializerProvider) => {
    gen.writeStartArray(value)
    gen.writeNumber(value.vertex1)
    gen.writeNumber(value.vertex2)
    gen.writeEndArray()
  })
  icfpc2021JsonModule.addDeserializer(classOf[Edge], (p: JsonParser, _: DeserializationContext) => {
    val x = p.nextIntValue(0)
    val y = p.nextIntValue(0)
    if (p.nextToken() != JsonToken.END_ARRAY)
      throw new JsonParseException(p, "Invalid end for Point")
    Edge(x, y)
  })

  private val mapper = new ObjectMapper()
  mapper.setPropertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE)
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(icfpc2021JsonModule)

  def parseProblem(content: String): Problem = {
    mapper.readValue[Problem](content, classOf[Problem])
  }

  def parseSolution(content: String): Solution = {
    mapper.readValue[Solution](content, classOf[Solution])
  }

  def toJson(value : Point) : String = {
    mapper.writeValueAsString((value.x, value.y))
  }

  def serializeSolution(solution: Solution): String = {
    mapper.writeValueAsString(solution)
  }
}
