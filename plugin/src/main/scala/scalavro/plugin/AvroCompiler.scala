package scalavro.plugin

import scalavro.schema.builder.AvroADTParser
import scalavro.schema.parser.AvscParser._
import io.circe.parser._
import scalavro.schema.Record

object AvroCompiler {
  def apply(str: String): String = {
    println(s"Compiling $str")
    decode[Record](str) match {
      case Right(record) =>
        println(s"Decoded as $record")
        AvroADTParser().buildAllClassesAsStr(record).mkString("\n")
      case Left(errs) =>
        throw new IllegalStateException(s"Unable to parse avsc ${str}, $errs")
    }
  }
}
