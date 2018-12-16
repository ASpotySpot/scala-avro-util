package scalavro.schema.types

import scalavro.schema.types.AvscType._

trait ToAvscType[A] {
  def apply: AvscType
}


object ToAvscType {
  def from[A](a: AvscType): ToAvscType[A] = new ToAvscType[A]{
    override def apply: AvscType = a
  }
  implicit val int: ToAvscType[Int] = from(IntType)
  implicit val str: ToAvscType[String] = from(StringType)
}