package scalavro.avsc

import scalavro.avsc.parser.JsonUtil._
import cats.data.{NonEmptyList, Validated}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Json, JsonObject}
import scalavro.avsc.parser.AvscParser.V

sealed trait AvscType {
  type DType
  def parseDefault(json: Json): V[DType]
}
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
case object NullType extends SimpleAvscType {
  override type DType = Null
  override def parseDefault(json: Json): V[NullType.DType] = Validated.valid(null)
}
case object BoolType extends SimpleAvscType {
  override type DType = Boolean
  override def parseDefault(json: Json): V[Boolean] = Validated.fromOption(json.asBoolean, s"$json is not a valid bool".toNel)
}
case object IntType extends SimpleAvscType {
  override type DType = Int
  override def parseDefault(json: Json): V[Int] = Validated.fromOption(json.asNumber.flatMap(_.toInt), s"$json is not a valid int".toNel)
}
case object LongType extends SimpleAvscType {
  override type DType = Long
  override def parseDefault(json: Json): V[Long] = Validated.fromOption(json.asNumber.flatMap(_.toLong), s"$json is not a valid long".toNel)
}
case object FloatType extends SimpleAvscType {
  override type DType = Float
  override def parseDefault(json: Json): V[Float] = Validated.fromOption(json.asNumber.map(_.toDouble.toFloat), s"$json is not a valid float".toNel)
}
case object DoubleType extends SimpleAvscType {
  override type DType = Double
  override def parseDefault(json: Json): V[Double] = Validated.fromOption(json.asNumber.map(_.toDouble), s"$json is not a valid double".toNel)
}
case object BytesType extends SimpleAvscType {
  override type DType = Array[Byte]
  override def parseDefault(json: Json): V[Array[Byte]] = ???
}
case object StringType extends SimpleAvscType {
  override type DType = String
  override def parseDefault(json: Json): V[String] = Validated.fromOption(json.asString, s"$json is not a valid string".toNel)
}
case class RecordByName(name: String) extends SimpleAvscType {
  override type DType = JsonObject
  override def parseDefault(json: Json): V[JsonObject] = Validated.fromOption(json.asObject, s"$json is not a valid string".toNel)
}


sealed trait ComplexAvscType extends AvscType {
  override type DType = JsonObject
  override def parseDefault(json: Json): V[JsonObject] = Validated.fromOption(json.asObject, s"$json is not a valid string".toNel)
}
case class Record(name: NonEmptyString,
                  namespace: Option[NonEmptyString],
                  doc: Option[String],
                  aliases: Option[List[NonEmptyString]],
                  fields: NonEmptyList[Field]) extends ComplexAvscType

case class Field(name: NonEmptyString,
                 doc: Option[String],
                 `type`: AvscType,
                 default: Option[Json]) {
  def getDefault: Option[`type`.DType] = default.map(_.asInstanceOf[`type`.DType])
}

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