package scalavro.plugin

import scalavro.builder.AvroADTParser
import scalavro.schema.parser.AvscParser._
import io.circe.parser._
import scalavro.schema.types.AvscType.Record

object AvroCompiler {
  def apply(str: String): String = {
    println(s"Decoding $str")
    decode[Record](str) match {
      case Right(record) =>
        println(s"Decoded $record")
        val result = AvroADTParser().buildAllClassesAsStr(record).mkString("\n")
        println(s"Generated Code $result")
        result
      case Left(errs) =>
        throw new IllegalStateException(s"Unable to parse avsc ${str}, $errs")
    }
  }
}
