package skm.parsers.usingpatternmatching

import zio.test._
import com.skm.parsers.usingpatternmatching.Parser
import com.skm.domains.Domains.Book
import zio.Chunk

object ParserSpec extends ZIOSpecDefault {

  val spec = suite("ParserSpec")(test("parse a book from a map of params") {
    for {
      res <-
        Parser
          .decode[Book]
          .provide(
            Parser.make(Map("title" -> Chunk("Good Omens"), "authors" -> Chunk("Neil Gaiman", "Terry Pratchett")))
          )
          .debug("results")
    } yield assertTrue(res.isRight)
  })
}
