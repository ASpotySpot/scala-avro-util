package scalavro.schema.parser

import java.nio.ByteBuffer

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe._
import io.circe.generic.AutoDerivation
import scalavro.util.IsoErr
import cats.syntax.traverse._
import cats.syntax.either._
import cats.syntax.apply._
import cats.instances.vector._
import scalavro.util.RefineUtils._
import JsonUtil._
import scalavro.schema.types.{AvscType, Utils}
import scalavro.schema.types.AvscType._

object AvscParser extends AutoDerivation {
  type JsonCodec[A] = IsoErr[V, A, Json]
  type V[A] = ValidatedNel[String, A]
  def jsonCodec[A](f: A => V[Json], g: Json => V[A]): JsonCodec[A] = IsoErr.from(f, g)

  implicit val dec: Decoder[Record] = Decoder[Json].emap{json =>
    Validated.
      fromOption(json.asObject, s"$json cannot be parsed as record".toNel).
      flatMap{obj =>
        Functions.toRecord(obj)
      }.toEither.leftMap(_.reduceLeft((a, b) => s"$a,$b"))
  }
  implicit val enc: Encoder[Record] = Encoder.instance[Record](r => Functions.fromRecord(r))

  private object Functions {

    def toSimple(j: String): SimpleAvscType = Utils.toSimple(j)
    def fromSimple(sat: SimpleAvscType): Json = Json.fromString(Utils.fromSimple(sat))
    def toRecord(jo: JsonObject): V[Record] = {
      val nameSpace: V[Option[NonEmptyString]] = jo.getStringOptNE("namespace")
      val name: V[NonEmptyString] = jo.getStringNE("name")
      val doc: V[Option[String]] = jo.getStringOpt("doc")
      val aliases: V[Option[List[NonEmptyString]]] = jo.getListStringOptNE("aliases").map(_.map(_.toList))
      val fields: V[NonEmptyList[Field]] = jo.getList("fields"){j =>
        Validated.
          fromOption(j.asObject, s"Invalid Field $j".toNel).
          map(toField).
          flatten
      }.flatMap(_.toNel)
      (name, nameSpace, doc, aliases, fields).mapN(Record.apply)
    }
    def fromRecord(r: Record): Json = {
      fromFields(
        List(
          "name" -> Json.fromString(r.name.value),
          "fields" -> Json.fromValues(r.fields.map(f => fromField(f)).toList),
          "type" -> Json.fromString("record")
        ), List(
          r.doc.map(d => "doc" -> Json.fromString(d)),
          r.aliases.map(as => "aliases" -> Json.fromValues(as.map(s => Json.fromString(s.value)))),
           r.namespace.map(ns => "namespace" -> Json.fromString(ns.value))
        )
      )
    }
    def toField(jo: JsonObject): V[Field] = {
      val name: V[NonEmptyString] = jo.getStringNE("name")
      val doc: V[Option[String]] = jo.getStringOpt("doc")
      val avscType: V[AvscType] = jo.get("type").flatMap(toType)
      (name, doc, avscType).mapN { case (n, d, t) =>
        jo("default").map { json =>
          Validated.fromOption(t.parseDefault(json), s"Invalid Default for $avscType".toNel)
        } match {
          case None => Validated.Valid(Field.apply(n,d,t)(None))
          case Some(vt) => vt.map(dt => Field.apply(n,d,t)(Some(dt)))
        }
      }.flatten
    }
    def fromField(f: Field): Json = {

      fromFields(
        List(
          "name" -> Json.fromString(f.name.value),
          "type" -> fromType(f.`type`)
        ),
        List(
          f.doc.map(doc => "doc" -> Json.fromString(doc)),
          DefaultFuncs.fromField(f)
        )
      )
    }
    object DefaultFuncs {
      def fromField(f: Field): Option[(String, Json)] = {
        f.default.map { d =>
          "default" -> inner(f.`type`)(d)
        }
      }
      private def inner(t: AvscType)(d: t.ScalaType): Json = t match {
        case NullType => Json.Null
        case BoolType => Json.fromBoolean(d.asInstanceOf[Boolean])
        case IntType => Json.fromInt(d.asInstanceOf[Int])
        case LongType => Json.fromLong(d.asInstanceOf[Long])
        case FloatType => Json.fromFloat(d.asInstanceOf[Float]).get
        case DoubleType => Json.fromDouble(d.asInstanceOf[Double]).get
        case StringType => Json.fromString(d.asInstanceOf[String])
        case BytesType => Json.fromValues(d.asInstanceOf[ByteBuffer].array().map(b => Json.fromInt(b.toInt)))
        case _: Fixed => Json.fromValues(d.asInstanceOf[Array[Byte]].map(b => Json.fromInt(b.toInt)))
        case ArrayType(items) =>
          val dd = d.asInstanceOf[List[items.ScalaType]]
          val col = dd.map(v => inner(items)(v))
          val r = Json.fromValues(col)
          r
        case MapType(values) =>
          val dd = d.asInstanceOf[Map[String, values.ScalaType]]
          val obj = JsonObject.fromMap(dd.map { case (k, v) => k -> inner(values)(v)})
          Json.fromJsonObject(obj)
        case Union(head, tail) =>
          val dd = d.asInstanceOf[head.ScalaType]
          inner(head)(dd)
        case _: EnumType => ???
        case _: Record => ???
        case _: RecordByName => ???
      }
    }
    def toEnum(jo: JsonObject): V[EnumType] = {
      val name = jo.getStringNE("name")
      val namespace = jo.getStringOptNE("namespace")
      val aliases: V[Option[List[NonEmptyString]]] = jo.getListStringOptNE("aliases").map(o => o.map(v => v.toList))
      val doc = jo.getStringOpt("doc")
      val symbols = jo.getList("symbols") { j =>
        Validated.
          fromOption(j.asString.map(_.refine), s"Field $j is not a valid symbol".toNel).
          flatten
      }.flatMap(_.toNel)
      (name, namespace, aliases, doc, symbols).mapN(EnumType.apply)
    }

    def fromEnum(enum: EnumType): Json = {
      fromFields(
        List(
          "name" -> Json.fromString(enum.name.value),
          "symbols" -> Json.fromValues(enum.symbols.map(s => Json.fromString(s.value)).toList),
          "type" -> Json.fromString("enum")
        ), List(
          enum.doc.map(d => "doc" -> Json.fromString(d)),
          enum.aliases.map(as => "aliases" -> Json.fromValues(as.map(s => Json.fromString(s.value)))),
          enum.namespace.map(ns => "namepsace" -> Json.fromString(ns.value))
        )
      )
    }
    def toArray(jo: JsonObject): V[ArrayType[_]] = {
      jo.get("items").flatMap{jsonType =>
        toType(jsonType).map(ArrayType.apply)
      }
    }
    def fromArray(arr: ArrayType[_ <: AvscType]): Json = {
      fromFields(
        "type" -> Json.fromString("array"),
        "items" -> fromType(arr.items)
      )
    }
    def toMap(jo: JsonObject): V[MapType[_ <: AvscType]] = {
      jo.get("values").flatMap{jsonType =>
        toType(jsonType).map(MapType.apply)
      }
    }
    def fromMap(map: MapType[_ <: AvscType]): Json = {
      fromFields(
        "type" -> Json.fromString("map"),
        "values" -> fromType(map.values)
      )
    }
    def toUnion(js: Vector[Json]): V[Union[_]] = {
      js.map(j => toType(j)).sequence[V, AvscType].flatMap{vs =>
        vs.toNel.map(nel => Union(nel.head, nel.tail))
      }
    }
    def fromUnion(union: Union[_]): Json = {
      Json.fromValues(union.types.map(fromType).toList)
    }
    def toFixed(jo: JsonObject): V[Fixed] = {
      val name: V[NonEmptyString] = jo.getStringNE("name")
      val namespace: V[Option[NonEmptyString]] = jo.getStringOptNE("namespace")
      val aliases: V[Option[List[NonEmptyString]]] = jo.getListStringOptNE("aliases").map(_.map(_.toList))
      val size: V[Int] = Validated.fromOption(
        jo("size").flatMap(_.asNumber.flatMap(_.toInt)),
        s"Field size not found as int".toNel
      )
      (name, namespace, aliases, size).mapN(Fixed.apply)
    }
    def fromFixed(fixed: Fixed): Json = fromFields(
      List(
        "name" -> Json.fromString(fixed.name.value),
        "size" -> Json.fromInt(fixed.size),
        "type" -> Json.fromString("fixed")
      ), List(
        fixed.namespace.map(ns => "namespace" -> Json.fromString(ns.value)),
        fixed.aliases.map(as => "aliases" -> Json.fromValues(as.map(s => Json.fromString(s.value)))),
      )
    )
    def fromType(avscType: AvscType): Json = avscType match {
      case sat: SimpleAvscType => fromSimple(sat)
      case rec: Record => fromRecord(rec)
      case enum: EnumType => fromEnum(enum)
      case arr: ArrayType[_] => fromArray(arr)
      case map: MapType[_] => fromMap(map)
      case union: Union[_] => fromUnion(union)
      case fixed: Fixed => fromFixed(fixed)
    }
    def toType(j: Json): V[AvscType] = j match {
      case j if j.isString => Validated.valid(toSimple(j.asString.get))
      case j if j.isArray  => toUnion(j.asArray.get)
      case j if j.isObject =>
        val obj = j.asObject.get
        obj.getStringNE("type").map(_.value).flatMap {
          case "record" => toRecord(obj)
          case "array" => toArray(obj)
          case "map" => toMap(obj)
          case "enum" => toEnum(obj)
          case "fixed" => toFixed(obj)
        }
    }
  }
}