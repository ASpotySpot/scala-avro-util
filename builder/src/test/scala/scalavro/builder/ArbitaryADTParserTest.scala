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

import scala.reflect.runtime.currentMirror
import scala.util.{Failure, Success, Try}

class ArbitaryADTParserTest extends FlatSpec with Checkers with Matchers {
  "AvroADT Parser" should "not crash with arbitary values" in {
    val parser = AvroADTParser.apply()
    check(forAll { r: Record =>
      val newR = r.copy(namespace = Some(refineMV[NonEmpty]("ns")))
      Try(parser.buildAllClassesAsStr(newR)).map(cleanCode) match {
        case Failure(ex) =>
          printBad(newR, None)
          throw ex

        case Success(code) =>
          Try(checkCompile(newR, code)) match {

            case Success(_) => true
            case Failure(ex) =>
              printBad(newR, Some(code))
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
      val code = cleanCode(parser.buildAllClassesAsStr(record))
      println(code)
      checkCompile(record, code)
    }
  }

  def printBad(r: Record, code: Option[String]): Unit = {
    println("=========")
    println(r)
    println(code.getOrElse(""))
    println("=========")
  }

  val codeLines = Array(
    "import shapeless.{:+:, CNil}",
    "class AsIndexedRecord(schema: String) extends scala.annotation.StaticAnnotation"
  )
  def cleanCode(code: List[String]): String = {
    val rawCode = code.mkString("\n").split('\n')
    val fixedDefs = codeLines ++ rawCode
    val droppedLines = fixedDefs.filterNot{line =>
      line.contains("import scalavro.macros.AsIndexedRecord")
    }
    droppedLines.mkString("\n")
  }
  def checkCompile(record: Record, code: String): Unit = {
    val classes = List(s"${record.namespace.get.value}.${record.name}")
    CompileTest.apply(code, classes)
//    val toolbox = currentMirror.mkToolBox()
//    val tree = toolbox.parse(code)
//    val compiledCode = toolbox.compile(tree)
//    val _ = compiledCode()
  }
}
