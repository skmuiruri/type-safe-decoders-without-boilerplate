package com.skm.parsers

import zio.{Chunk, ZIOAppDefault, ZLayer}
import com.skm.parsers.simple.QueryParamsParser._

object Main extends ZIOAppDefault {

  val params: Map[String, String] = Map("name" -> "John", "surname" -> "Doe", "age" -> "42")

  def run =
    simple.QueryParamsParser.decode[Person].debug("results: ")
      .provide(simple.QueryParamsParser.make(params))

}
