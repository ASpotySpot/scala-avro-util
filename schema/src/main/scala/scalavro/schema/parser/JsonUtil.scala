package scalavro.schema.parser

import cats.data.Validated
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Json, JsonObject}
import cats.syntax.traverse._
import cats.instances.vector._
import scalavro.schema.parser.AvscParser.V
import scalavro.util.RefineUtils._

object JsonUtil {

  implicit class RichObj(jsonObject: JsonObject) {
    def get(name: String): V[Json] = Validated.fromOption(jsonObject(name), s"Unable to get field $name".toNel)
    def getStringNE(name: String): V[NonEmptyString] = {
      Validated.fromOption(jsonObject(name).flatMap(_.asString), s"Unable to get field $name as string".toNel).flatMap(_.refine)
    }
    def getString(name: String): V[String] = {
      Validated.fromOption(jsonObject(name).flatMap(_.asString), s"Unable to get field $name as string".toNel)
    }
    def getStringOptNE(name: String): V[Option[NonEmptyString]] = getStringOpt(name).flatMap{
      case None => Validated.valid(None)
      case Some(s) => s.refine.map(Some(_))
    }
    def getStringOpt(name: String): V[Option[String]] = {
      jsonObject(name) match {
        case None =>
          Validated.valid(None)
        case Some(json) =>
          Validated.fromOption(
            json.asString.map(Some(_)),
            s"Unable to get field $name as string".toNel
          )
      }
    }
    def getList[A](name: String)(f: Json => V[A]): V[Vector[A]] = {
      Validated.
        fromOption(jsonObject(name).flatMap(_.asArray), s"Unable to get field $name as array".toNel).
        flatMap{js =>
          js.map(f).sequence[V, A]
        }
    }
    def getListString(name: String): V[Vector[String]] = {
      Validated.fromOption(
        jsonObject(name).flatMap(_.asArray),
        s"Field $name is not a valid array".toNel
      ).flatMap{vs =>
        vs.map{j =>
          Validated.fromOption(j.asString, s"$j is not a valid string".toNel)
        }.sequence[V, String]
      }
    }
    def getListStringOpt(name: String): V[Option[Vector[String]]] = {
      jsonObject(name) match {
        case None => Validated.valid(None)
        case Some(json) => Validated.fromOption(
          json.asArray,
          s"Field $name is not a valid array".toNel
        ).flatMap{vs =>
          vs.map{j =>
            Validated.fromOption(j.asString, s"$j is not a valid string".toNel)
          }.sequence[V, String]
        }.map(Some(_))
      }
    }
    def getListStringOptNE(name: String): V[Option[Vector[NonEmptyString]]] = {
      jsonObject(name) match {
        case None => Validated.valid(None)
        case Some(json) => Validated.fromOption(
          json.asArray,
          s"Field $name is not a valid array".toNel
        ).flatMap{vs =>
          vs.map{j =>
            Validated.fromOption(j.asString, s"$j is not a valid string".toNel).
              flatMap(_.refine)
          }.sequence[V, NonEmptyString]
        }.map(Some(_))
      }
    }
  }

  def fromFields(iter: (String, Json)*): Json = {
    Json.fromJsonObject(JsonObject.fromIterable(iter))
  }
  def fromFields(iter: List[(String, Json)], iterO: List[Option[(String, Json)]]): Json = {
    Json.fromJsonObject(JsonObject.fromIterable(iterO.foldLeft(iter){(soFar, next) =>
      next.fold(soFar)(n => n :: soFar)
    }))
  }
  def fromFields2(iter: (String, Json)*)(iterO: Option[(String, Json)]*): Json = {
    Json.fromJsonObject(JsonObject.fromIterable(iterO.foldLeft(iter.toList){(soFar, next) =>
      next.fold(soFar)(n => n :: soFar)
    }))
  }
}
