package com.skm.parsers

import com.skm.parsers.simple.QueryParamsParser._
import zio.{Chunk, ZIOAppDefault}

object Main extends ZIOAppDefault {

  val params: Map[String, List[String]] = Map(
    "title" -> List("Good Omens"),
    "authors" -> List("Neil Gaiman", "Terry Pratchett"),
//    "Moby-Dick" -> List("Herman Melville"),
//    "For Whom the bell toils" -> Chunk("Oscar wilde"),
//    "Good Omens" -> Chunk("Neil Gaiman", "Terry Pratchett"),
//    "Divine Comedy" -> Chunk("Dante Alighieri")
  )

  /*
  def collect[E, A](value: Chunk[Either[E, A]]): Either[E, Chunk[A]] =
    value.foldLeft[Either[E, Chunk[A]]](Right(Chunk.empty)) {
      case (Left(e), _)      => Left(e)
      case (Right(chunk), either) =>
        either match {
          case Left(value)  => Left(value)
          case Right(value) => Right(chunk :+ value)
        }
    }
   */
  def collect[E, A](values: Chunk[Either[E, A]]): Either[E, Chunk[A]] = {
    values.foldLeft[Either[E, Chunk[A]]](Right(Chunk.empty[A])) {
      case (Left(e), _) => Left(e)
      case (Right(chunk), either) =>
        either match {
          case Left(e) => Left(e)
          case Right(value) => Right(chunk :+ value)
        }
    }
  }

  def run =
    simple.QueryParamsParser
      .decode[Book]
      .debug("results: ")
      .provide(simple.QueryParamsParser.make(params))
}