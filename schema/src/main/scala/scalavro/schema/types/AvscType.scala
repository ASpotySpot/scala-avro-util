package scalavro.schema.types

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Json, JsonObject}

sealed trait AvscType {
  type ScalaType
  def parseDefault(json: Json): Option[ScalaType]
}

object AvscType {
  sealed trait SimpleAvscType extends AvscType
  sealed trait ComplexAvscType extends AvscType

  case object NullType extends SimpleAvscType {
    override type ScalaType = Null
    override def parseDefault(json: Json): Null = null
  }
  case object BoolType extends SimpleAvscType {
    override type ScalaType = Boolean
    override def parseDefault(json: Json): Option[Boolean] = json.asBoolean
  }
  case object IntType extends SimpleAvscType {
    override type ScalaType = Int
    override def parseDefault(json: Json): Option[Int] = json.asNumber.flatMap(_.toInt)
  }
  case object LongType extends SimpleAvscType {
    override type ScalaType = Long
    override def parseDefault(json: Json): Option[Long] = json.asNumber.flatMap(_.toLong)
  }
  case object FloatType extends SimpleAvscType {
    override type ScalaType = Float
    override def parseDefault(json: Json): Option[Float] = json.asNumber.map(_.toDouble).map(_.toFloat)
  }
  case object DoubleType extends SimpleAvscType {
    override type ScalaType = Double
    override def parseDefault(json: Json): Option[Double] = json.asNumber.map(_.toDouble)
  }
  case object BytesType extends SimpleAvscType {
    override type ScalaType = ByteBuffer
    override def parseDefault(json: Json): Option[ByteBuffer] = json.asString.map(_.map(_.toByte).toArray).map(ByteBuffer.wrap)
  }
  case object StringType extends SimpleAvscType {
    override type ScalaType = String
    override def parseDefault(json: Json): Option[String] = json.asString
  }
  case class RecordByName(name: NonEmptyString) extends SimpleAvscType {
    override type ScalaType = JsonObject
    override def parseDefault(json: Json): Option[JsonObject] = json.asObject
  }

  case class Record(name: NonEmptyString,
                    namespace: Option[NonEmptyString],
                    doc: Option[String],
                    aliases: Option[List[NonEmptyString]],
                    fields: NonEmptyList[Field]) extends ComplexAvscType {
    override type ScalaType = JsonObject
    override def parseDefault(json: Json): Option[JsonObject] = json.asObject
  }

  class Field private(val name: NonEmptyString,
                      val doc: Option[String],
                      val `type`: AvscType) {
    def default: Option[`type`.ScalaType] = None
    override def toString: String = s"Field($name, $doc, ${`type`})($default)"
  }
  object Field {
    def apply(name: NonEmptyString,
             doc: Option[String],
             `type`: AvscType)(
              defaultParm: Option[`type`.ScalaType]): Field = new Field(name, doc, `type`) {
      override val default: Option[`type`.ScalaType] = defaultParm.map(_.asInstanceOf[`type`.ScalaType])
    }
  }

  case class EnumType(name: NonEmptyString,
                      namespace: Option[NonEmptyString],
                      aliases: Option[List[NonEmptyString]],
                      doc: Option[String],
                      symbols: NonEmptyList[NonEmptyString]) extends ComplexAvscType {
    override type ScalaType = String
    override def parseDefault(json: Json): Option[String] = json.asString
  }

  case class ArrayType[X <: AvscType](items: X) extends ComplexAvscType {
    override type ScalaType = List[items.ScalaType]
    override def parseDefault(json: Json): Option[List[items.ScalaType]] = {
      val x: Option[Vector[Option[items.ScalaType]]] = json.asArray.map(_.map(j => items.parseDefault(j)))
      x.flatMap(_.foldLeft(Option(Vector.empty[items.ScalaType])) {
        case (Some(soFar), Some(next)) => Some(soFar :+ next)
        case _ => None
      }).map(_.toList)
    }
  }

  case class MapType(values: AvscType) extends ComplexAvscType {
    override type ScalaType = Map[String, values.ScalaType]
    override def parseDefault(json: Json): Option[Map[String, values.ScalaType]] =
      json.asObject.flatMap(_.toMap.map { case (k, v) =>
        values.parseDefault(v).map(vt => k -> vt)
      }.foldLeft(Option(Vector.empty[(String, values.ScalaType)])) {
        case (Some(soFar), Some(next)) => Some(soFar :+ next)
        case _ => None
      }).map(_.toMap)
  }

  case class Union(types: NonEmptyList[AvscType]) extends ComplexAvscType {
    override type ScalaType = types.head.ScalaType
    override def parseDefault(json: Json): Option[types.head.ScalaType] = types.head.parseDefault(json)
  }

  case class Fixed(name: NonEmptyString,
                   namespace: Option[NonEmptyString],
                   aliases: Option[List[NonEmptyString]],
                   size: Int) extends ComplexAvscType {
    override type ScalaType = ByteBuffer
    override def parseDefault(json: Json): Option[ByteBuffer] = json.asString.map(_.map(_.toByte).toArray).map(ByteBuffer.wrap)
  }
}








