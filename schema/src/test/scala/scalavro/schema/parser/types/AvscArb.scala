package scalavro.schema.parser.types

import cats.data.NonEmptyList
import io.circe.parser._
import io.circe.syntax._
import org.scalacheck.{Arbitrary, Gen}
import scalavro.schema.{DoubleType, Field, Record}
//import org.scalacheck.Prop.forAll
//import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import scalavro.schema.parser.AvscParser._

class AvscArb extends FlatSpec with Checkers with Matchers {
  implicit def f: String => NonEmptyString = s => refineV[NonEmpty](s).right.get
  implicit val arbString: Arbitrary[NonEmptyString] = Arbitrary{
    Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString).map(s => refineV[NonEmpty](s).right.get)
  }
  "Records" should "be convertable to and from json" in {
//    check(forAll{r: Record =>
//      val json = r.asJson.noSpaces
//      try {
//        decode[Record](json) match {
//          case Right(a) if a == r => true
//          case Right(a) =>
//            println("NOT EQUAL")
//            println(json)
//            println(a.asJson.noSpaces)
//            println(r)
//            println(a)
//            false
//          case Left(err) =>
//            println("ERROR")
//            println(json)
//            println(r)
//            println(err)
//            false
//        }
//      } catch {
//        case e: Exception =>
//          println("CRASH")
//          println(json)
//          println(r)
//          throw e
//      }
//
//    })
  }
  it should "run this manual test" in {
    val r1 = Record("f1",None,None,None,NonEmptyList(Field("f2",None,DoubleType), List.empty))
    val json = r1.asJson.noSpaces
    val r2 = decode[Record](json) match {
      case Right(v) => v
      case Left(err) =>
        println(err)
        ???
    }
    println(s"Input: ${r1.fields.head.`type`.getClass}")
    println(s"Result: ${r2.fields.head.`type`.getClass}")
    r1 shouldEqual r2
  }
}