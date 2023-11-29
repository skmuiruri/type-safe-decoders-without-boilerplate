package com.skm.domains

import zio.schema.{ DeriveSchema, Schema }

object Domains {
  case class Book(title: String, authors: List[String])

  object Book {
    implicit val schema: Schema[Book] = DeriveSchema.gen[Book]
  }
}
