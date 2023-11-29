package com.skm.parsers.usingdynamicvalue

import zio.schema.DynamicValue._
import zio.schema._
import zio.{ Chunk, ZIO, ZLayer }

import scala.collection.immutable.ListMap
import scala.util.Try

final class QueryParamsParser private (params: Map[String, List[String]]) {
  def decode[A](implicit schema: Schema[A]): scala.util.Either[String, A] =
    toDynamicValue(params)
      .map(_.toTypedValue(schema))
      .collectFirst { case Right(v) => v }
      .toRight("error decoding the provided values")

  private def toDynamicValue(params: Map[String, List[String]]): Set[DynamicValue] =
    params.foldLeft[Set[ListMap[String, DynamicValue]]](Set(ListMap())) {
      case (set, (key, values)) =>
        set.flatMap { acc =>
          values match {
            case Nil      =>
              Set(acc.updated(key, Singleton(())))
            case x :: Nil =>
              val strInterpretation =
                Set(acc.updated(key, Primitive[String](x, StandardType.StringType)))
              val intInterpretation = Try(x.toInt).toOption match {
                case Some(value) =>
                  Set(acc.updated(key, Primitive[Int](value, StandardType.IntType)))
                case None        => Set()
              }
              strInterpretation ++ intInterpretation
            case xs       =>
              Set(
                acc.updated(
                  key,
                  DynamicValue.Sequence(
                    Chunk.fromIterable(xs).map(Primitive[String](_, StandardType.StringType))
                  )
                )
              )
          }
        }
      }
      .map(v => DynamicValue.Record(TypeId.Structural, v))
}

object QueryParamsParser {
  def make(queryParams: Map[String, List[String]]): ZLayer[Any, Nothing, QueryParamsParser] =
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
