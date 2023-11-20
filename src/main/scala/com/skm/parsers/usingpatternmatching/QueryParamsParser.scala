package com.skm.parsers.usingpatternmatching

import zio.schema.{ DynamicValue, Schema, StandardType, TypeId }
import zio.{ Chunk, Unsafe }

import scala.collection.immutable.ListMap
import scala.util.Try

final class QueryParamsParser(map: Map[String, Chunk[String]]) {
  def decode[A](implicit schema: Schema[A]): Either[String, A] =
    (schema.asInstanceOf[Schema[_]] match {
      case enum: Schema.Enum[_]                                  => Left("Enum not supported")
      case collection: Schema.Collection[_, _]                   => Left("Collection not supported")
      case Schema.Optional(schema, annotations)                  => Right(decode(schema).toOption)
      case Schema.Lazy(schema0)                                  => decode(schema0())
      case record: Schema.Record[_]                              => decodeRecord(record)
      case Schema.Transform(schema, f, g, annotations, identity) => decode(schema).flatMap(f.asInstanceOf[Any => Either[String, Any]])
      case Schema.Primitive(standardType, annotations)           => Left("Primitive not supported")
      case Schema.Dynamic(annotations)                           => decodeDynamicValue
      case _                                                     => Left("This schema cannot be decoded from query params") // add schema info to erro message
    }).map(_.asInstanceOf[A])

  private def collect[E, A](value: Chunk[Either[E, A]]): Either[E, Chunk[A]] =
    value.foldLeft[Either[E, Chunk[A]]](Right(Chunk.empty)) {
      case (Left(e), _)           => Left(e)
      case (Right(chunk), either) =>
        either match {
          case Left(value)  => Left(value)
          case Right(value) => Right(chunk :+ value)
        }
    }

  private def decodeRecord[A](record: Schema.Record[_]): Either[String, Either[String, _]] = {
    val fieldValues: Chunk[Either[String, Any]] = record.fields.map { field =>
      val fieldValue = map.get(field.name)
      fieldValue match {
        case Some(queryParamValues) => decodeFieldValue(queryParamValues, field, field.schema)
        case None                   => Left(s"Expected a value for field ${field.name}")
      }
    }
    for {
      value <- collect(fieldValues)
    } yield record.construct(value)(Unsafe.unsafe[A])
  }

  private def decodeDynamicValue: Right[Nothing, DynamicValue.Record] =
    Right(
      DynamicValue.Record(
        TypeId.Structural,
        ListMap(map.map { case (paramName, values) =>
          (paramName, DynamicValue.Primitive(values.headOption.getOrElse(""), StandardType.StringType))
        }.toSeq: _*)
      )
    )

  private def toX[R](fieldName: String, expectedType: String, queryParamValue: String)(
    f: String => Option[R]
  ): Either[String, R] =
    f(queryParamValue)
      .map(Right(_))
      .getOrElse(Left(s"Expected a $expectedType but found $queryParamValue for field $fieldName"))

  private def managePrimitiveType(
    params: Chunk[String],
    field: Schema.Field[_, _],
    standardType: StandardType[Any]
  ): Either[String, _] =
    if (params.isEmpty)
      Left(s"Expected a String but no query parameter value found for ${field.name}")
    else if (params.size > 1)
      Left(s"Expected a String but found multiple values for ${field.name}")
    else {
      val queryParamValue = params(0)

      standardType match {
        case StandardType.UnitType           => Right(())
        case StandardType.StringType         => Right(params(0))
        case StandardType.BoolType           =>
          toX(field.name, StandardType.BoolType.tag, queryParamValue)(_.toBooleanOption)
        case StandardType.ByteType           =>
          toX(field.name, StandardType.ByteType.tag, queryParamValue)(_.toByteOption)
        case StandardType.ShortType          =>
          toX(field.name, StandardType.ShortType.tag, queryParamValue)(_.toShortOption)
        case StandardType.IntType            =>
          toX(field.name, StandardType.IntType.tag, queryParamValue)(_.toIntOption)
        case StandardType.LongType           =>
          toX(field.name, StandardType.LongType.tag, queryParamValue)(_.toLongOption)
        case StandardType.FloatType          =>
          toX(field.name, StandardType.FloatType.tag, queryParamValue)(_.toFloatOption)
        case StandardType.DoubleType         =>
          toX(field.name, StandardType.DoubleType.tag, queryParamValue)(_.toDoubleOption)
        case StandardType.BinaryType         => Left("Binary Not Supported") // impose regulation and document it
        case StandardType.CharType           => Right(params(0).head)
        case StandardType.UUIDType           =>
          toX(field.name, StandardType.UUIDType.tag, queryParamValue)(v => Try(java.util.UUID.fromString(v)).toOption)
        case StandardType.BigDecimalType     =>
          toX(field.name, StandardType.BigDecimalType.tag, queryParamValue)(v => Try(BigDecimal(v)).toOption)
        case StandardType.BigIntegerType     =>
          toX(field.name, StandardType.BigIntegerType.tag, queryParamValue)(v => Try(BigInt(v)).toOption)
        case StandardType.DayOfWeekType      =>
          toX(field.name, StandardType.DayOfWeekType.tag, queryParamValue)(v =>
            Try(java.time.DayOfWeek.valueOf(v.toUpperCase())).toOption
          )
        case StandardType.MonthType          =>
          toX(field.name, StandardType.MonthType.tag, queryParamValue)(v =>
            Try(java.time.Month.valueOf(v.toUpperCase())).toOption
          )
        case StandardType.MonthDayType       =>
          toX(field.name, StandardType.MonthDayType.tag, queryParamValue)(v =>
            Try(java.time.MonthDay.parse(v)).toOption
          )
        case StandardType.PeriodType         =>
          toX(field.name, StandardType.PeriodType.tag, queryParamValue)(v => Try(java.time.Period.parse(v)).toOption)
        case StandardType.YearType           =>
          toX(field.name, StandardType.YearType.tag, queryParamValue)(v => Try(java.time.Year.parse(v)).toOption)
        case StandardType.YearMonthType      =>
          toX(field.name, StandardType.YearMonthType.tag, queryParamValue)(v =>
            Try(java.time.YearMonth.parse(v)).toOption
          )
        case StandardType.ZoneIdType         =>
          toX(field.name, StandardType.ZoneIdType.tag, queryParamValue)(v => Try(java.time.ZoneId.of(v)).toOption)
        case StandardType.ZoneOffsetType     =>
          toX(field.name, StandardType.ZoneOffsetType.tag, queryParamValue)(v =>
            Try(java.time.ZoneOffset.of(v)).toOption
          )
        case StandardType.DurationType       =>
          toX(field.name, StandardType.DurationType.tag, queryParamValue)(v =>
            Try(java.time.Duration.parse(v)).toOption
          )
        case StandardType.InstantType        =>
          toX(field.name, StandardType.InstantType.tag, queryParamValue)(v => Try(java.time.Instant.parse(v)).toOption)
        case StandardType.LocalDateType      =>
          toX(field.name, StandardType.LocalDateType.tag, queryParamValue)(v =>
            Try(java.time.LocalDate.parse(v)).toOption
          )
        case StandardType.LocalTimeType      =>
          toX(field.name, StandardType.LocalTimeType.tag, queryParamValue)(v =>
            Try(java.time.LocalTime.parse(v)).toOption
          )
        case StandardType.LocalDateTimeType  =>
          toX(field.name, StandardType.LocalDateTimeType.tag, queryParamValue)(v =>
            Try(java.time.LocalDateTime.parse(v)).toOption
          )
        case StandardType.OffsetTimeType     =>
          toX(field.name, StandardType.OffsetTimeType.tag, queryParamValue)(v =>
            Try(java.time.OffsetTime.parse(v)).toOption
          )
        case StandardType.OffsetDateTimeType =>
          toX(field.name, StandardType.OffsetDateTimeType.tag, queryParamValue)(v =>
            Try(java.time.OffsetDateTime.parse(v)).toOption
          )
        case StandardType.ZonedDateTimeType  =>
          toX(field.name, StandardType.ZonedDateTimeType.tag, queryParamValue)(v =>
            Try(java.time.ZonedDateTime.parse(v)).toOption
          )
      }
    }

  def decodeFieldValue(params: Chunk[String], field: Schema.Field[_, _], schema: Schema[_]): Either[String, _] =
    schema match {
      case collection: Schema.Collection[_, _]                   =>
        collection match {
          case Schema.Sequence(elementSchema, fromChunk, toChunk, annotations, identity) =>
            params.map(v => decodeFieldValue(Chunk(v), field, elementSchema)).foldLeft[Either[String, Chunk[Any]]](Right(Chunk.empty)) {
            case (Left(e), either) => Left(e)
            case (Right(chunk), either) =>
              either match {
                case Left(value) => Left(value)
                case Right(value) => Right(chunk :+ value)
              }
          }.map(fromChunk)
          case Schema.Map(keySchema, valueSchema, annotations)                           => Left("Map not supported")
          case Schema.Set(elementSchema, annotations)                                    =>
            collect(params.map(v => decodeFieldValue(Chunk(v), field, elementSchema)))
              .map(Chunk.fromIterable(_).toSet)
        }
      case Schema.Transform(schema, f, g, annotations, identity) =>
        decodeFieldValue(params, field, schema).flatMap(f.asInstanceOf[Any => Either[String, Any]])
      case Schema.Primitive(standardType, annotations)           =>
        managePrimitiveType(params, field, standardType.asInstanceOf[StandardType[Any]])
      case Schema.Optional(schema, annotations)                  => Right(decodeFieldValue(params, field, schema).toOption)
      case Schema.Tuple2(left, right, annotations)               =>
        if (params.size == 2) {
          val leftValue  = decodeFieldValue(Chunk(params(0)), field, left)
          val rightValue = decodeFieldValue(Chunk(params(1)), field, right)
          for {
            left  <- leftValue
            right <- rightValue
          } yield (left, right)
        } else Left(s"Expected a Tuple2 but found ${params.size} values for ${field.name}")

      case Schema.Lazy(schema0)        => decodeFieldValue(params, field, schema0())
      case Schema.Dynamic(annotations) =>
        Right(DynamicValue.Sequence(params.map(DynamicValue.Primitive(_, StandardType.StringType))))
      case _                           => Left("This schema cannot be decoded from query params") // add schema info to erro message
    }
}

object QueryParamsParser {}
