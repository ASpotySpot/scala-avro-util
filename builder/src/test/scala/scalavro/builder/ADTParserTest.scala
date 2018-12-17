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

class ADTParserTest extends FlatSpec with Matchers {
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
         |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"default\":3,\"name\":\"a\",\"type\":\"int\"},{\"default\":\"defaultVal\",\"name\":\"s\",\"type\":\"string\"},{\"name\":\"f\",\"type\":\"float\"},{\"default\":[1,2,3],\"name\":\"bts\",\"type\":\"bytes\"}],\"type\":\"record\"}") case class MyClass(var a: Int = 3, var s: String = "defaultVal", var f: Float, var bts: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3))) {
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
          Field(refineMV[NonEmpty]("j"), None, ArrayType(IntType))(Some(List(1, 2, 1))),
          Field(refineMV[NonEmpty]("k"), None, MapType(IntType))(Some(Map("a" -> 1, "b" -> 2, "c" -> 1))),
          Field(refineMV[NonEmpty]("l"), None, Union(IntType, List(StringType)))(Some(2)),
          Field(refineMV[NonEmpty]("m"), None, Fixed(refineMV[NonEmpty]("fixed-name"), None, None, 6))(Some(Array(1,2,3,4,5,6)))
        )
      )
    )
    val expected ="""|package tomw {
                     |  import scalavro.macros.AsIndexedRecord
                     |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"default\":null,\"name\":\"a\",\"type\":\"null\"},{\"default\":true,\"name\":\"b\",\"type\":\"boolean\"},{\"default\":12,\"name\":\"c\",\"type\":\"int\"},{\"default\":9,\"name\":\"d\",\"type\":\"long\"},{\"default\":3.7,\"name\":\"e\",\"type\":\"float\"},{\"default\":3.6,\"name\":\"f\",\"type\":\"double\"},{\"default\":\"defaultVal\",\"name\":\"g\",\"type\":\"string\"},{\"default\":[1,2,3],\"name\":\"h\",\"type\":\"bytes\"},{\"default\":[null],\"name\":\"i\",\"type\":{\"type\":\"array\",\"items\":\"null\"}},{\"default\":[1,2,1],\"name\":\"j\",\"type\":{\"type\":\"array\",\"items\":\"int\"}},{\"default\":{\"a\":1,\"b\":2,\"c\":1},\"name\":\"k\",\"type\":{\"type\":\"map\",\"values\":\"int\"}},{\"default\":2,\"name\":\"l\",\"type\":[\"int\",\"string\"]},{\"default\":[1,2,3,4,5,6],\"name\":\"m\",\"type\":{\"name\":\"fixed-name\",\"size\":6,\"type\":\"fixed\"}}],\"type\":\"record\"}") case class MyClass(var a: Null = null, var b: Boolean = true, var c: Int = 12, var d: Long = 9L, var e: Float = 3.7F, var f: Double = 3.6, var g: String = "defaultVal", var h: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)), var i: Array[Null] = Array(null), var j: Array[Int] = Array(1, 2, 1), var k: Map[String, Int] = Map(scala.Tuple2("a", 1), scala.Tuple2("b", 2), scala.Tuple2("c", 1)), var l: :+:[String, :+:[Int, CNil]] = Inr(Inl(2)), var m: Array[Byte] = scala.Array(1, 2, 3, 4, 5, 6)) {
                     |    def this() = {
                     |      this(null, true, 12, 9L, 3.7F, 3.6, "defaultVal", java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)), Array(null), Array(1, 2, 1), Map(scala.Tuple2("a", 1), scala.Tuple2("b", 2), scala.Tuple2("c", 1)), Inr(Inl(2)), scala.Array(1, 2, 3, 4, 5, 6));
                     |      ()
                     |    }
                     |  }
                     |}""".stripMargin
    val result = build(input)
    println(result.filterNot(_ == '\n'))
    println(expected.filterNot(_ == '\n'))
    result shouldEqual expected
    CustomCompiler.apply(result, "tomw.MyClass")
  }
}



