package scalavro.schema.types

import java.nio.ByteBuffer
import java.time.{LocalDate, ZoneId, ZoneOffset}

import scalavro.schema.types.AvscType.{BytesType, IntType}

trait LogicalType[T <: AvscType, L] {
  def to(t: T#ScalaType): L
  def from(t: L): T#ScalaType
}

object LogicalType {
  class IdentLT[T <: AvscType] extends LogicalType[T, T#ScalaType] {
    override def to(t: T#ScalaType): T#ScalaType = t
    override def from(t: T#ScalaType): T#ScalaType = t
  }
  object DateLT extends LogicalType[IntType.type, java.time.LocalDate] {
    override def to(t: Int): LocalDate = LocalDate.ofEpochDay(t.toLong)
    override def from(t: LocalDate): Int = t.toEpochDay.toInt
  }

  object DecimalLT extends LogicalType[BytesType.type, BigDecimal] {
    override def to(t: ByteBuffer): BigDecimal = ???
    override def from(t: BigDecimal): ByteBuffer = ???
  }
}
