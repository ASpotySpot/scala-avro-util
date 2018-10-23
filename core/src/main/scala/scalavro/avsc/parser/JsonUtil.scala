package scalavro.avsc.parser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Json, JsonObject}
import scalavro.avsc.parser.AvscParser.V
import cats.syntax.traverse._
import cats.instances.vector._
object JsonUtil {

  implicit class FlatMapVal[A](v: V[A]) {
    def flatMap[B](f: A => V[B]): V[B] = v match {
      case Valid(a) => f(a)
      case Invalid(nel) => Invalid(nel)
    }
    def flatten[B](implicit ev: A <:< V[B]): V[B] = v.flatMap(ev)
  }
  implicit class RichList[A](ls: Vector[A]) {
    def toNel: V[NonEmptyList[A]] = if(ls.isEmpty) {
      Validated.invalid(s"List is empty".toNel)
    } else {
      Validated.valid(NonEmptyList(ls.head, ls.tail.toList))
    }
  }
  implicit class RichString(s: String) {
    def refine: V[NonEmptyString] = {
      Validated.fromEither(refineV[NonEmpty](s)).leftMap(_.toNel)
    }
    def toNel: NonEmptyList[String] = NonEmptyList(s, List.empty)
  }

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
}
