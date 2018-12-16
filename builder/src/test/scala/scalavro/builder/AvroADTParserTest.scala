package scalavro.builder

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import eu.timepit.refined.collection.NonEmpty
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.refineMV
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic._
import scalavro.schema.parser.AvscParser._
import scalavro.schema.types.AvscType._

class AvroADTParserTest extends FlatSpec with Matchers {
  private def build(rec: Record): String = AvroADTParser().buildAllClassesAsStr(rec).mkString("\n")

  "AvroADTParser" should "parse this and preserve ordering" in {
    val input = Record(
      refineMV[NonEmpty]("MyClass"),
      Some(refineMV[NonEmpty]("tomw")),
      None,
      None,
      NonEmptyList(
        Field(refineMV[NonEmpty]("a"), None, IntType)(Some(3)),
        List(
          Field(refineMV[NonEmpty]("s"), None, StringType)(Some("defaultVal")),
          Field(refineMV[NonEmpty]("f"), None, FloatType)(None),
          Field(refineMV[NonEmpty]("bts"), None, BytesType)(Some(ByteBuffer.wrap(Array(1,2,3))))
        )
      )
    )

    val expected =
      """|package tomw {
         |  import scalavro.macros.AsIndexedRecord
         |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"name\":\"a\",\"type\":\"int\"},{\"name\":\"s\",\"type\":\"string\"},{\"name\":\"f\",\"type\":\"float\"},{\"name\":\"bts\",\"type\":\"bytes\"}],\"type\":\"record\"}") case class MyClass(var a: Int = 3, var s: String = "defaultVal", var f: Float, var bts: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3))) {
         |    def this() = {
         |      this(3, "defaultVal", -1, java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)));
         |      ()
         |    }
         |  }
         |}""".stripMargin

    val result = build(input)
    result shouldEqual expected
  }

  it should "handle cases with Maps" in {
    val input = Record(
      refineMV[NonEmpty]("MyClass"),
      Some(refineMV[NonEmpty]("tomw")),
      None,
      None,
      NonEmptyList(
        Field(refineMV[NonEmpty]("a"), None, MapType(IntType))(None),
        List.empty
      )
    )
    val expected ="""|package tomw {
       |  import scalavro.macros.AsIndexedRecord
       |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"name\":\"a\",\"type\":{\"type\":\"map\",\"values\":\"int\"}}],\"type\":\"record\"}") case class MyClass(var a: Map[String, Int]) {
       |    def this() = {
       |      this(Map.empty[String, Int]);
       |      ()
       |    }
       |  }
       |}""".stripMargin

    val result = AvroADTParser().buildAllClassesAsStr(input).head
    result shouldEqual expected
  }

  it should "build defaults for all types" in {
    val input = Record(
      refineMV[NonEmpty]("MyClass"),
      Some(refineMV[NonEmpty]("tomw")),
      None,
      None,
      NonEmptyList(
        Field(refineMV[NonEmpty]("a"), None, NullType)(Some(null)),
        List(
          Field(refineMV[NonEmpty]("b"), None, BoolType)(Some(true)),
          Field(refineMV[NonEmpty]("c"), None, IntType)(Some(12)),
          Field(refineMV[NonEmpty]("d"), None, LongType)(Some(9L)),
          Field(refineMV[NonEmpty]("e"), None, FloatType)(Some(3.7f)),
          Field(refineMV[NonEmpty]("f"), None, DoubleType)(Some(3.6d)),
          Field(refineMV[NonEmpty]("g"), None, StringType)(Some("defaultVal")),
          Field(refineMV[NonEmpty]("h"), None, BytesType)(Some(ByteBuffer.wrap(Array(1, 2, 3)))),
          Field(refineMV[NonEmpty]("i"), None, ArrayType(NullType))(Some(List(null))),
          Field(refineMV[NonEmpty]("j"), None, ArrayType(IntType))(Some(List(1, 2, 1)))
        )
      )
    )
    val expected ="""|package tomw {
                     |  import scalavro.macros.AsIndexedRecord
                     |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"default\":null,\"name\":\"a\",\"type\":\"null\"},{\"default\":true,\"name\":\"b\",\"type\":\"boolean\"},{\"default\":12,\"name\":\"c\",\"type\":\"int\"},{\"default\":9,\"name\":\"d\",\"type\":\"long\"},{\"default\":3.7,\"name\":\"e\",\"type\":\"float\"},{\"default\":3.6,\"name\":\"f\",\"type\":\"double\"},{\"default\":\"defaultVal\",\"name\":\"g\",\"type\":\"string\"},{\"default\":[1,2,3],\"name\":\"h\",\"type\":\"bytes\"},{\"default\":[null],\"name\":\"i\",\"type\":{\"type\":\"array\",\"items\":\"null\"}},{\"default\":[1,2,1],\"name\":\"j\",\"type\":{\"type\":\"array\",\"items\":\"int\"}}],\"type\":\"record\"}") case class MyClass(var a: Null = null, var b: Boolean = true, var c: Int = 12, var d: Long = 9L, var e: Float = 3.7F, var f: Double = 3.6, var g: String = "defaultVal", var h: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)), var i: Array[Null] = Array(null), var j: Array[Int] = Array(1, 2, 1)) {
                     |    def this() = {
                     |      this(null, true, 12, 9L, 3.7F, 3.6, "defaultVal", java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)), Array(null), Array(1, 2, 1));
                     |      ()
                     |    }
                     |  }
                     |}""".stripMargin
    val result = build(input)
//    println(result.filterNot(_ == '\n'))
//    println(expected.filterNot(_ == '\n'))
    result shouldEqual expected
  }
}



