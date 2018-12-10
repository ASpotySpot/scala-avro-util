package scalavro.plugin

import java.io.PrintWriter

import sbt.Keys._
import sbt._
import sbt.util.FileInfo
import scalavro.schema.parser.AvscParser._

import scala.io.Source

object ScalavroSBT extends AutoPlugin {
  override def requires = empty

  override def trigger = allRequirements

  object autoImport {
    val scalavroTask = taskKey[Seq[File]]("ScalavroGenerate")
    val avroSrcDir = settingKey[Seq[File]]("Directories of Avro Schemas")
    val avroTgtDir = settingKey[File]("Directory to output files to")
    val cacheGenerate = settingKey[Boolean]("Directory to output files to")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    avroSrcDir := Seq(sourceDirectory.value / "avro"),
    avroTgtDir := sourceManaged.value / "compiled_avro",
    cacheGenerate := false,
    commands += Command.command("scalavro", "help-scalavro", "scalavro-detail") { state =>
      state.copy(remainingCommands = Exec("scalavroTask", None) +: state.remainingCommands)
    },
    scalavroTask := {
      val tgt = avroTgtDir.value
      if(!tgt.exists()) tgt.mkdirs()
      val cacheFunc = if (cacheGenerate.value) {
        FileFunction.cached(target.value / "scalavro", FileInfo.lastModified, FileInfo.hash) _
      } else {
        identity[Set[File] => Set[File]] _
      }
      val inputs = avroSrcDir.value.flatMap(_.listFiles()).toSet
      cacheFunc(compile(_, tgt))(inputs).toSeq
    }
  )

  private def compile(files: Set[File], target: File): Set[File] = {
    files.map{f =>
      val outputFile = target / s"${f.getName.split('.').dropRight(1).mkString(".")}.scala"
      println(s"Compiling Avsc '$f' to '$target")
      val pw = new PrintWriter(outputFile)
      pw.println(AvroCompiler(Source.fromFile(f).mkString))
      pw.flush()
      pw.close()
      outputFile
    }
  }
}
