//package scalavro.builder
//
//import eu.timepit.refined.collection.NonEmpty
//import org.scalatest.{FlatSpec, Matchers}
//import eu.timepit.refined.{refineMV, refineV}
//import eu.timepit.refined.types.string.NonEmptyString
//import org.scalatest.prop.Checkers
//import org.scalacheck.{Arbitrary, Gen}
//import org.scalacheck.Prop.forAll
//import org.scalacheck.ScalacheckShapeless._
//
//import tools.reflect.ToolBox
//import scalavro.schema.types.AvscType
//import scalavro.schema.types.AvscType._
//
//
//
//import scala.reflect.runtime.currentMirror
//import scala.util.{Failure, Success, Try}
//
//class ArbitaryADTParserTest extends FlatSpec with Checkers with Matchers {
//  implicit val arbString: Arbitrary[NonEmptyString] = Arbitrary{
//    Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString).map(s => refineV[NonEmpty](s).right.get)
//  }
//  def s(t: AvscType): Arbitrary[t.ScalaType] = {
//    def cast(g: Gen[_]): Arbitrary[t.ScalaType] = Arbitrary(g.map(_.asInstanceOf[t.ScalaType]))
//    val z = t match {
//      case NullType => cast(Gen.const(null))
//      case BoolType => cast(Arbitrary.arbitrary[Boolean])
//      case IntType => cast(Arbitrary.arbitrary[Int])
//      case LongType => cast(Arbitrary.arbitrary[Long])
//      case FloatType => cast(Arbitrary.arbitrary[Float])
//      case DoubleType => cast(Arbitrary.arbitrary[Double])
//      case StringType => cast(Arbitrary.arbitrary[String])
//      case BytesType => cast(Arbitrary.arbitrary[Array[Byte]])
//      case ArrayType(tt) => cast(s(tt).arbitrary)
//      case MapType(tt) => cast(s(tt).arbitrary)
//      case Union(tts) => cast(s(tts.head).arbitrary)
//      case _: Fixed => cast(Arbitrary.arbitrary[Array[Byte]])
//      case _: EnumType => cast(Arbitrary.arbitrary[String])
//      case _ => cast(Gen.const(null))
//    }
//    z
//  }
//  implicit val fGen: Arbitrary[Field] = Arbitrary{for {
//    name <- arbString.arbitrary
//    doc <- Gen.option(arbString.arbitrary).map(_.map(_.value))
//    at <- Arbitrary.arbitrary[AvscType]
//    d <- s(at).arbitrary.map(Some(_))
//  } yield Field(name,doc,at)(d)}
//
//
//  "AvroADT Parser" should "not crash with arbitary values" in {
//    val parser = AvroADTParser.apply()
//    check(forAll{r: Record =>
//      val newR = if(r.namespace.isEmpty) r.copy(namespace = Some(refineMV[NonEmpty]("ns"))) else r
//      val code = parser.buildAllClassesAsStr(newR).mkString("\n")
//      Try {
//        val toolbox = currentMirror.mkToolBox()
//        val _ = toolbox.parse(code)
//        println(code)
//        //        val compiledCode = toolbox.compile(tree)
//        //        compiledCode()
//      } match {
//        case Success(_) => //
//        case Failure(ex) =>
//          println("=========")
//          println(newR)
//          println(code)
//          throw ex
//      }
//      true
//    })
//  }
//}
