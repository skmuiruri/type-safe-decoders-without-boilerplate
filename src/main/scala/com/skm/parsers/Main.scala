package com.skm.parsers

import com.skm.domains.Domains.Book
import zio.{ Chunk, ZIOAppDefault }

object Main extends ZIOAppDefault {

  val paramChunks: Map[String, Chunk[String]] = Map(
    "title"   -> Chunk("Good Omens"),
    "authors" -> Chunk("Neil Gaiman", "Terry Pratchett")
  )

  val params: Map[String, List[String]] = Map(
    "title"   -> List("Good Omens"),
    "authors" -> List("Neil Gaiman", "Terry Pratchett")
  )

  def run =
//    usingdynamicvalue.QueryParamsParser
//      .decode[Book]
//      .provide(usingdynamicvalue.QueryParamsParser.make(params))

    usingpatternmatching.QueryParamsParser
      .decode[Book]
      .debug("results")
      .provide(usingpatternmatching.QueryParamsParser.make(paramChunks))

}
