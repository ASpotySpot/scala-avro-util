package scalavro.util

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import scalavro.schema.parser.AvscParser.V

object RefineUtils {
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
    def refineF: NonEmptyString = Validated.fromEither(refineV[NonEmpty](s)).leftMap(_.toNel).getOrElse(throw new Exception("Invalid Get"))

    def toNel: NonEmptyList[String] = NonEmptyList(s, List.empty)
  }
}
