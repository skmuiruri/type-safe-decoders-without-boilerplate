package com.skm.parsers.usingdynamicvalue

import zio.schema.DynamicValue._
import zio.schema._
import zio.{ Chunk, ZIO, ZLayer }

import scala.collection.immutable.ListMap
import scala.util.Try

final class SimpleParser private (params: Map[String, List[String]]) {
  def decode[A](implicit schema: Schema[A]): scala.util.Either[String, A] =
    toDynamicValue(params)
      .map(_.toTypedValue(schema))
      .collectFirst { case Right(v) => v }
      .toRight("error decoding the provided values")

  private def toDynamicValue(params: Map[String, List[String]]): Set[DynamicValue] =
    params
      .foldLeft[Set[ListMap[String, DynamicValue]]](Set(ListMap())) { case (set, (key, values)) =>
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

object SimpleParser {
  def make(queryParams: Map[String, List[String]]): ZLayer[Any, Nothing, SimpleParser] =
    ZLayer(ZIO.succeed(new SimpleParser(queryParams)))

  def decode[A](implicit schema: Schema[A]): ZIO[SimpleParser, Nothing, Either[String, A]] =
    ZIO.serviceWith[SimpleParser](_.decode)
}
