package scalavro.schema

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString

sealed trait AvscType

object AvscType {
  sealed trait AvscOrder
  case object Ascending extends AvscOrder
  case object Descending extends AvscOrder

  def simple(typeStr: String): SimpleAvscType = typeStr match {
    case "null" => NullType
    case "boolean" => BoolType
    case "int" => IntType
    case "long" => LongType
    case "float" => FloatType
    case "double" => DoubleType
    case "bytes" => BytesType
    case "string" => StringType
    case recName => RecordByName(recName)
  }

  def fromScala(typeStr: String): AvscType = typeStr match {
    case "Null" => NullType
    case "Boolean" => BoolType
    case "Int" => IntType
    case "Long" => LongType
    case "Float" => FloatType
    case "Double" => DoubleType
    case "String" => StringType
    case "ByteBuffer" | "java.nio.ByteBuffer" => BytesType
  }
}

sealed trait SimpleAvscType extends AvscType {
  override def toString: String = this match {
    case NullType => "null"
    case BoolType => "boolean"
    case IntType => "int"
    case LongType => "long"
    case FloatType => "float"
    case DoubleType => "double"
    case BytesType => "bytes"
    case StringType => "string"
    case RecordByName(name) => name
  }
}
case object NullType extends SimpleAvscType
case object BoolType extends SimpleAvscType
case object IntType extends SimpleAvscType
case object LongType extends SimpleAvscType
case object FloatType extends SimpleAvscType
case object DoubleType extends SimpleAvscType
case object BytesType extends SimpleAvscType
case object StringType extends SimpleAvscType
case class RecordByName(name: String) extends SimpleAvscType

sealed trait ComplexAvscType extends AvscType
case class Record(name: NonEmptyString,
                  namespace: Option[NonEmptyString],
                  doc: Option[String],
                  aliases: Option[List[NonEmptyString]],
                  fields: NonEmptyList[Field]) extends ComplexAvscType

case class Field(name: NonEmptyString,
                 doc: Option[String],
                 `type`: AvscType)

case class EnumType(name: NonEmptyString,
                    namespace: Option[NonEmptyString],
                    aliases: Option[List[NonEmptyString]],
                    doc: Option[String],
                    symbols: NonEmptyList[NonEmptyString]) extends ComplexAvscType

case class ArrayType(items: AvscType) extends ComplexAvscType
case class MapType(values: AvscType) extends ComplexAvscType
case class Union(types: NonEmptyList[AvscType]) extends ComplexAvscType
case class Fixed(name: NonEmptyString,
                 namespace: Option[NonEmptyString],
                 aliases: Option[List[NonEmptyString]],
                 size: Int) extends ComplexAvscType