package scalavro.builder

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import org.scalatest.{Assertion, FlatSpec, Matchers}
import scalavro.schema.types.AvscType._
import RefineUtils._
import io.circe.{Json, JsonObject}

class ADTParserTest extends FlatSpec with Matchers {
  private def build(rec: Record): String = AvroADTParser().buildAllClassesAsStr(rec).mkString("\n")

  "AvroADTParser" should "parse this and preserve ordering" in {
    val input = Record(
      "MyClass",
      Some("tomw"),
      None,
      None,
      NonEmptyList(
        Field("a", None, IntType)(Some(3)),
        List(
          Field("s", None, StringType)(Some("defaultVal")),
          Field("f", None, FloatType)(None),
          Field("bts", None, BytesType)(Some(ByteBuffer.wrap(Array(1,2,3))))
        )
      )
    )

    val expected =
      """|package tomw {
         |  import scalavro.macros.AsIndexedRecord
         |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"default\":3,\"name\":\"a\",\"type\":\"int\"},{\"default\":\"defaultVal\",\"name\":\"s\",\"type\":\"string\"},{\"name\":\"f\",\"type\":\"float\"},{\"default\":\"\\\\u010203\",\"name\":\"bts\",\"type\":\"bytes\"}],\"type\":\"record\"}") case class MyClass(var a: Int = 3, var s: String = "defaultVal", var f: Float, var bts: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3))) {
         |    def this() = {
         |      this(3, "defaultVal", -1, java.nio.ByteBuffer.wrap(scala.Array(1, 2, 3)));
         |      ()
         |    }
         |  }
         |}""".stripMargin

    val result = build(input)
    testEqual(result, expected)
  }

  it should "build null types" in {
    testSingleField(
      Field("a", None, NullType)(None),
      """{\"name\":\"a\",\"type\":\"null\"}""",
      """var a: Null""",
      """null"""
    )
    testSingleField(
        Field("a", None, NullType)(Some(null)),
    """{\"default\":null,\"name\":\"a\",\"type\":\"null\"}""",
    """var a: Null = null""",
    """null"""
    )
  }

  it should "build bool types" in {
    testSingleField(
      Field("a", None, BoolType)(None),
      """{\"name\":\"a\",\"type\":\"boolean\"}""",
      """var a: Boolean""",
      """false"""
    )
    testSingleField(
        Field("a", None, BoolType)(Some(true)),
    """{\"default\":true,\"name\":\"a\",\"type\":\"boolean\"}""",
    """var a: Boolean = true""",
    """true"""
    )
  }

  it should "build int types" in {
    testSingleField(
      Field("a", None, IntType)(None),
      """{\"name\":\"a\",\"type\":\"int\"}""",
      """var a: Int""",
      """-1"""
    )
    testSingleField(
        Field("a", None, IntType)(Some(67)),
    """{\"default\":67,\"name\":\"a\",\"type\":\"int\"}""",
    """var a: Int = 67""",
    """67"""
    )
  }

  it should "build long types" in {
    testSingleField(
      Field("a", None, LongType)(None),
      """{\"name\":\"a\",\"type\":\"long\"}""",
      """var a: Long""",
      """-1"""
    )
    testSingleField(
        Field("a", None, LongType)(Some(-12)),
    """{\"default\":-12,\"name\":\"a\",\"type\":\"long\"}""",
    """var a: Long = -12L""",
    """-12L"""
    )
  }

  it should "build float types" in {
    testSingleField(
      Field("a", None, FloatType)(None),
      """{\"name\":\"a\",\"type\":\"float\"}""",
      """var a: Float""",
      """-1"""
    )
    testSingleField(
        Field("a", None, FloatType)(Some(-12.6f)),
    """{\"default\":-12.6,\"name\":\"a\",\"type\":\"float\"}""",
    """var a: Float = -12.6F""",
    """-12.6F"""
    )
  }

  it should "build double types" in {
    testSingleField(
      Field("a", None, DoubleType)(None),
      """{\"name\":\"a\",\"type\":\"double\"}""",
      """var a: Double""",
      """-1"""
    )
    testSingleField(
        Field("a", None, DoubleType)(Some(12.6)),
    """{\"default\":12.6,\"name\":\"a\",\"type\":\"double\"}""",
    """var a: Double = 12.6""",
    """12.6"""
    )
  }

  it should "build string types" in {
    testSingleField(
      Field("a", None, StringType)(None),
      """{\"name\":\"a\",\"type\":\"string\"}""",
      """var a: String""",
      """null"""
    )
    testSingleField(
        Field("a", None, StringType)(Some("Hi Mom Im in a Test")),
    """{\"default\":\"Hi Mom Im in a Test\",\"name\":\"a\",\"type\":\"string\"}""",
    """var a: String = "Hi Mom Im in a Test"""",
    """"Hi Mom Im in a Test""""
    )
  }

  it should "build bytes types" in {
    testSingleField(
      Field("a", None, BytesType)(None),
      """{\"name\":\"a\",\"type\":\"bytes\"}""",
      """var a: java.nio.ByteBuffer""",
      """null"""
    )
    testSingleField(
        Field("a", None, BytesType)(Some(ByteBuffer.wrap(Array(-123,120,9)))),
    """{\"default\":\"\\\\u857809\",\"name\":\"a\",\"type\":\"bytes\"}""",
    """var a: java.nio.ByteBuffer = java.nio.ByteBuffer.wrap(scala.Array(-123, 120, 9))""",
    """java.nio.ByteBuffer.wrap(scala.Array(-123, 120, 9))"""
    )
  }

  it should "build array types" in {
    testSingleField(
      Field("a", None, ArrayType(IntType))(None),
      """{\"name\":\"a\",\"type\":{\"type\":\"array\",\"items\":\"int\"}}""",
      """var a: Array[Int]""",
      """null"""
    )
    testSingleField(
        Field("a", None, ArrayType(IntType))(Some(List(1,2,1))),
    """{\"default\":[1,2,1],\"name\":\"a\",\"type\":{\"type\":\"array\",\"items\":\"int\"}}""",
    """var a: Array[Int] = Array(1, 2, 1)""",
    """Array(1, 2, 1)"""
    )
  }

  it should "build map types" in {
    testSingleField(
      Field("a", None, MapType(BoolType))(None),
      """{\"name\":\"a\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}}""",
      """var a: Map[String, Boolean]""",
      """Map.empty[String, Boolean]"""
    )
    testSingleField(
        Field("a", None, MapType(BoolType))(Some(Map("a" -> true, "b" -> false))),
    """{\"default\":{\"a\":true,\"b\":false},\"name\":\"a\",\"type\":{\"type\":\"map\",\"values\":\"boolean\"}}""",
    """var a: Map[String, Boolean] = Map(scala.Tuple2("a", true), scala.Tuple2("b", false))""",
    """Map(scala.Tuple2("a", true), scala.Tuple2("b", false))"""
    )
  }

  it should "build union types" in {
    testSingleField(
      Field("a", None, Union(IntType, List(StringType)))(None),
      """{\"name\":\"a\",\"type\":[\"int\",\"string\"]}""",
      """var a: :+:[String, :+:[Int, CNil]]""",
      """null"""
    )
    testSingleField(
        Field("a", None, Union(IntType, List(StringType)))(Some(3)),
    """{\"default\":3,\"name\":\"a\",\"type\":[\"int\",\"string\"]}""",
    """var a: :+:[String, :+:[Int, CNil]] = Inr(Inl(3))""",
    """Inr(Inl(3))"""
    )
  }

  it should "build fixed types" in {
    testSingleField(
      Field("a", None, Fixed("fixed-name", None, None, 4))(None),
      """{\"name\":\"a\",\"type\":{\"name\":\"fixed-name\",\"size\":4,\"type\":\"fixed\"}}""",
      """var a: Array[Byte]""",
      """null"""
    )
    testSingleField(
      Field("a", None, Fixed("fixed-name", None, None, 4))(Some(Array(-5,3,0,1))),
      """{\"default\":[-5,3,0,1],\"name\":\"a\",\"type\":{\"name\":\"fixed-name\",\"size\":4,\"type\":\"fixed\"}}""",
      """var a: Array[Byte] = scala.Array(-5, 3, 0, 1)""",
      """scala.Array(-5, 3, 0, 1)"""
    )
  }

  it should "build enum types" in {
    val input = Record(
      "MyClass",
      Some("tomw"),
      None,
      None,
      List(
        Field("a", None, EnumType("EnumName", None, None, None, NonEmptyList("A", List("B", "C"))))(Some("B"))
      )
    )
    val expected =
      """|package tomw {
         |  import scalavro.macros.AsIndexedRecord
         |  sealed trait EnumName
         |  case object A extends EnumName with scala.Product with scala.Serializable
         |  case object B extends EnumName with scala.Product with scala.Serializable
         |  case object C extends EnumName with scala.Product with scala.Serializable
         |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[{\"default\":\"B\",\"name\":\"a\",\"type\":{\"name\":\"EnumName\",\"symbols\":[\"A\",\"B\",\"C\"],\"type\":\"enum\"}}],\"type\":\"record\"}") case class MyClass(var a: tomw.EnumName = EnumName.fromString("B")) {
         |    def this() = {
         |      this(EnumName.fromString("B"));
         |      ()
         |    }
         |  }
         |}""".stripMargin

    val result = build(input)
    testEqual(result, expected)
  }

  it should "build record types" in {
    val input = Record(
      "Outer",
      Some("tomw"),
      None,
      None,
      List(
        Field(
          "inner", None, Record(
            "Inner",
            None,
            None,
            None,
            List(Field("i", None, IntType)(None))
          )
        )(None)
      )
    )
    val expected =  """|package tomw {
                       |  import scalavro.macros.AsIndexedRecord
                       |  @AsIndexedRecord("{\"name\":\"Inner\",\"fields\":[{\"name\":\"i\",\"type\":\"int\"}],\"type\":\"record\"}") case class Inner(var i: Int) {
                       |    def this() = {
                       |      this(-1);
                       |      ()
                       |    }
                       |  }
                       |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"Outer\",\"fields\":[{\"name\":\"inner\",\"type\":{\"name\":\"Inner\",\"fields\":[{\"name\":\"i\",\"type\":\"int\"}],\"type\":\"record\"}}],\"type\":\"record\"}") case class Outer(var inner: tomw.Inner) {
                       |    def this() = {
                       |      this(null);
                       |      ()
                       |    }
                       |  }
                       |}""".stripMargin
    val result = build(input)
    testEqual(result, expected)
  }

  it should "build record types with defaults" in {
    val input = Record(
      "Outer",
      Some("tomw"),
      None,
      None,
      List(
        Field(
          "inner", None, Record(
            "Inner",
            None,
            None,
            None,
            List(Field("i", None, IntType)(Some(4)))
          )
        )(Some(JsonObject.fromMap(Map(
          "i" -> Json.fromInt(3)
          ))
        ))
      )
    )
    val expected =  """|package tomw {
                       |  import scalavro.macros.AsIndexedRecord
                       |  @AsIndexedRecord("{\"name\":\"Inner\",\"fields\":[{\"default\":4,\"name\":\"i\",\"type\":\"int\"}],\"type\":\"record\"}") case class Inner(var i: Int = 4) {
                       |    def this() = {
                       |      this(4);
                       |      ()
                       |    }
                       |  }
                       |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"Outer\",\"fields\":[{\"default\":{\"i\":3},\"name\":\"inner\",\"type\":{\"name\":\"Inner\",\"fields\":[{\"default\":4,\"name\":\"i\",\"type\":\"int\"}],\"type\":\"record\"}}],\"type\":\"record\"}") case class Outer(var inner: tomw.Inner = Inner(i = 3)) {
                       |    def this() = {
                       |      this(Inner(i = 3));
                       |      ()
                       |    }
                       |  }
                       |}""".stripMargin
    val result = build(input)
    testEqual(result, expected)
  }

  it should "build recursive record types" in {
    val input = Record(
      "Person",
      Some("tomw"),
      None,
      None,
      List(
        Field(
          "children",
          None,
          ArrayType(RecordByName("Person"))
        )(None)
      )
    )
    val expected = """|package tomw {
                      |  import scalavro.macros.AsIndexedRecord
                      |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"Person\",\"fields\":[{\"name\":\"children\",\"type\":{\"type\":\"array\",\"items\":\"Person\"}}],\"type\":\"record\"}") case class Person(var children: Array[tomw.Person]) {
                      |    def this() = {
                      |      this(null);
                      |      ()
                      |    }
                      |  }
                      |}""".stripMargin

    val result = build(input)
    testEqual(result, expected)
  }


  def testSingleField(f: Field, expectedAvsc: String, expectedDef: String, expectedDefault: String): Assertion = {
    val input = Record(
      "MyClass",
      Some("tomw"),
      None,
      None,
      List(
        f
      )
    )
    val s1: String =
     """|package tomw {
        |  import scalavro.macros.AsIndexedRecord
        |  @AsIndexedRecord("{\"namespace\":\"tomw\",\"name\":\"MyClass\",\"fields\":[""".stripMargin
    val s2: String = """],\"type\":\"record\"}") case class MyClass("""
    val s3: String =
     """|) {
        |    def this() = {
        |      this(""".stripMargin
    val s4: String = """);
                       |      ()
                       |    }
                       |  }
                       |}""".stripMargin
    val expected = List(s1,expectedAvsc,s2,expectedDef,s3,expectedDefault,s4).mkString

    val result = build(input)
    testEqual(result, expected)
  }

  private def testEqual(r: String, e: String): Assertion = {
    val cleanRes = r.filterNot(_ == '\n')
    val cleanExp = e.filterNot(_ == '\n')
    if(r != e) {
      println(cleanRes)
      println(cleanExp)
    }
    r shouldEqual e
  }
}



