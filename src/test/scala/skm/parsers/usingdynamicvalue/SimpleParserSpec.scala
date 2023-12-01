package skm.parsers.usingdynamicvalue

import zio.test.ZIOSpecDefault
import com.skm.parsers.usingdynamicvalue.SimpleParser
import com.skm.domains.Domains.Book
import zio.test._

object SimmpleParserSpec extends ZIOSpecDefault {
  val spec = (suite("SimpleParserSpec")(
    test("parse a book from a map of params") {
      for {
        res <-
          SimpleParser
            .decode[Book]
            .provide(
              SimpleParser.make(Map("title" -> List("Good Omens"), "authors" -> List("Neil Gaiman", "Terry Pratchett")))
            )
            .debug("results")
      } yield assertTrue(res.isRight)
    }
  ))
}
