package scalavro.builder

import eu.timepit.refined.collection.NonEmpty
import org.scalatest.{FlatSpec, Matchers}
import eu.timepit.refined.{refineMV, refineV}
import eu.timepit.refined.types.string.NonEmptyString
import org.scalatest.prop.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.ScalacheckShapeless._

import tools.reflect.ToolBox
import scalavro.schema.types.AvscType
import scalavro.schema.types.AvscType._
import ArbAvsc._
import cats.data.NonEmptyList

import scala.util.{Failure, Success, Try}

class ArbitaryADTParserTest extends FlatSpec with Checkers with Matchers {
  "AvroADT Parser" should "not crash with arbitary values" in {
    val parser = AvroADTParser.apply()
    check(forAll { r: Record =>
      val newR = r.copy(namespace = Some(refineMV[NonEmpty]("ns")))
      Try(parser.buildAllClassesAsStr(newR)) match {
        case Failure(ex) =>
          println(newR)
          throw ex

        case Success(code) =>
          Try(checkCompile(newR, code)) match {

            case Success(_) => true
            case Failure(ex) =>
              println(newR)
              throw ex
          }
      }
    })
  }

  implicit def nestr(s: String): NonEmptyString = refineV[NonEmpty](s).right.get
  implicit def nels[A](ls: List[A]): NonEmptyList[A] = NonEmptyList(ls.head, ls.tail)
  //This is just here to allow easy recreating of cases failed above test.
  it should "recreate single example" in {
    val record: Option[Record] = None

    record.foreach { record =>
      val parser = AvroADTParser.apply()
      val code = parser.buildAllClassesAsStr(record)
      checkCompile(record, code)
    }
  }

  def checkCompile(record: Record, code: List[String]): Unit = {
    CustomCompiler.apply(code, s"${record.namespace.get.value}.${record.name}")
  }
}
