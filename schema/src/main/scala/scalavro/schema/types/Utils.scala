package scalavro.schema.types

import scalavro.schema.types.AvscType._

object Utils {
  def toSimple(typeStr: String): SimpleAvscType = typeStr match {
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
  def fromSimple(avscType: SimpleAvscType): String = avscType match {
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
