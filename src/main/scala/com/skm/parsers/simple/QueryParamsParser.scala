package com.skm.parsers.simple

import zio.schema._
import zio.{ Chunk, ZIO, ZLayer }

import scala.collection.immutable.ListMap

final class QueryParamsParser private (queryParams: Map[String, String]) {

  def decode[A](implicit schema: Schema[A]): Either[String, A] = {
    val record = DynamicValue.Record(
      TypeId.Structural,
      ListMap(queryParams.map { case (paramName, values) =>
        (paramName, DynamicValue.Primitive(values.headOption.getOrElse(""), StandardType.StringType))
      }.toSeq: _*)
    )
    schema.fromDynamic(record)
  }
}

object QueryParamsParser {

  case class Person(name: String, surname: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  def make(queryParams: Map[String, String]): ZLayer[Any, Nothing, QueryParamsParser] =
    ZLayer(ZIO.succeed(new QueryParamsParser(queryParams)))

  def decode[A](implicit schema: Schema[A]): ZIO[QueryParamsParser, Nothing, Either[String, A]] =
    ZIO.serviceWith[QueryParamsParser](_.decode)
}

/*
 GET /api/books/search?query=programming
&filter=category:computing
&filter=author:john_doe
&sort=publication_date:desc
&pagination=page:2&pageSize:10

query=programming: Simple key-value pair for the search query.
filter=category:computing and filter=author:john_doe: Complex parameters indicating filters. Each filter consists of a key-value pair, where the key represents the filter type (category, author) and the value is the specific criterion (computing, john_doe).
sort=publication_date:desc: Sorting parameter where publication_date is the field to sort by, and desc indicates descending order.
pagination=page:2&pageSize:10: Pagination parameters specifying that the user wants to see the second page with a page size of 10.

 */
