//package scalavro.schema.parser.types
//
//import cats.data.NonEmptyList
//import io.circe.parser._
//import io.circe.syntax._
//import org.scalacheck.{Arbitrary, Gen}
//import scalavro.schema._
//import org.scalacheck.Prop.forAll
//import org.scalacheck.ScalacheckShapeless._
//import org.scalatest.{FlatSpec, Matchers}
//import org.scalatest.prop.Checkers
//import eu.timepit.refined._
//import eu.timepit.refined.collection.NonEmpty
//import eu.timepit.refined.types.string.NonEmptyString
//import scalavro.schema.parser.AvscParser._
//import org.apache.avro.Schema
//
//import scala.util.{Failure, Success, Try}
//
//class AvscArb extends FlatSpec with Checkers with Matchers {
//  implicit def f: String => NonEmptyString = s => refineV[NonEmpty](s).right.get
//  implicit val arbString: Arbitrary[NonEmptyString] = Arbitrary{
//    Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString).map(s => refineV[NonEmpty](s).right.get)
//  }
//  "Records" should "be convertable to and from json" in {
//  check(forAll{r: Record =>
//    val json = r.asJson.noSpaces
////    new Schema.Parser().parse(json)
//    try {
//      decode[Record](json) match {
//        case Right(a) if a == r => true
//        case Right(a) =>
//          println("NOT EQUAL")
//          println(json)
//          println(a.asJson.noSpaces)
//          println(r)
//          println(a)
//          false
//        case Left(err) =>
//          println("ERROR")
//          println(json)
//          println(r)
//          println(err)
//          false
//      }
//    } catch {
//      case e: Exception =>
//        println("CRASH")
//        println(json)
//        println(r)
//        throw e
//    }
//
//  })
//  }
//  it should "run this manual test" in {
//    val r1 = Record(
//      "mtvjkqapvuilq",
//      None,
//      None,
//      None,
//      NonEmptyList(
//        Field("eg",None, NullType),
//        List(
//          Field("p",None, BoolType),
//          Field("ht",None,BoolType),
//          Field("yx",None,FloatType),
//          Field("k",Some(""), MapType(LongType)),
//          Field("dn",None,BoolType))
//      )
//    )
//
//    val json = r1.asJson.noSpaces
//    val check1 = Try(new Schema.Parser().parse(json)).toEither
//    val check2 = decode[Record](json)
//    (check1, check2) match {
//      case (Right(s), Right(r)) =>
//        println("G,G")
//        println(s)
//        println(r)
//        r shouldEqual r1
//      case (Left(ex), Left(err)) =>
//        println("B,B")
//        println(json)
//        println(err)
//        throw ex
//      case (Right(s), Left(err)) =>
//        println("G,B")
//        println(json)
//        println(err)
//        println(s)
//        false shouldEqual true
//      case (Left(ex), Right(r)) =>
//        println("B,G")
//        println(json)
//        println(r)
//        throw ex
//    }
//  }
//
//  it should "retain ordering" in {
//    val schemaJson = "{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"name\":\"a\",\"type\":\"int\"},{\"name\":\"s\",\"type\":\"string\"},{\"name\":\"f\",\"type\":\"float\"},{\"name\":\"bts\",\"type\":\"bytes\"}],\"type\":\"record\"}"
//    val record = decode[Record](schemaJson)
//    val expected = List(
//      Field("a", None, IntType),
//      Field("s", None, StringType),
//      Field("f", None, FloatType),
//      Field("bts", None, BytesType)
//    )
//    record.right.get.fields.toList should contain theSameElementsInOrderAs expected
//  }
//}