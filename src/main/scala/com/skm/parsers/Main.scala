package com.skm.parsers

import com.skm.parsers.usingdynamicvalue.QueryParamsParser._
import zio.{ Chunk, ZIOAppDefault }

object Main extends ZIOAppDefault {

  val params: Map[String, List[String]] = Map(
    "title"   -> List("Good Omens"),
    "authors" -> List("Neil Gaiman", "Terry Pratchett")
//    "Moby-Dick" -> List("Herman Melville"),
//    "For Whom the bell toils" -> Chunk("Oscar wilde"),
//    "Good Omens" -> Chunk("Neil Gaiman", "Terry Pratchett"),
//    "Divine Comedy" -> Chunk("Dante Alighieri")
  )

  def run =
    usingdynamicvalue.QueryParamsParser
      .decode[Book]
      .debug("results: ")
      .provide(usingdynamicvalue.QueryParamsParser.make(params))
}
