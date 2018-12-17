package scalavro.builder

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import tools.nsc.io.VirtualDirectory
import scala.reflect.internal.util.AbstractFileClassLoader
import java.security.MessageDigest
import java.math.BigInteger

import collection.mutable
import scala.util.{Failure, Success, Try}

object CustomCompiler {
  private val codeLines = Array(
    "import shapeless.{:+:, CNil, Inr, Inl}",
    "class AsIndexedRecord(schema: String)"
  )

  private def cleanCode(code: List[String]): String = {
    val rawCode = code.mkString("\n").split('\n')
    val fixedDefs = codeLines ++ rawCode
    val droppedLines = fixedDefs.filterNot{line =>
      line.contains("import scalavro.macros.AsIndexedRecord")
    }
    droppedLines.mkString("\n")
  }

  def apply(str: List[String], clazz: String): Unit = {
    (new CustomCompiler).compile(cleanCode(str), clazz)
  }
  def apply(str: String, clazz: String): Unit = {
    apply(List(str), clazz)
  }
}

class CustomCompiler {
  val target = new VirtualDirectory("(memory)", None)
  val classCache = mutable.Map[String, Class[_]]()

  private val settings = new Settings()
  settings.outputDirs.setSingleOutput(target)
  settings.usejavacp.value = true
   val global = new Global(settings)
  lazy val run = new global.Run

  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  /**Compiles the code as a class into the class loader of this compiler.
    *
    * @param code
    * @return
    */
  def compile(code: String, clazz: String) = {
    val sourceFiles = List(new BatchSourceFile("(inline)", code))
    try{
      run.compileSources(sourceFiles) //Works in IntelliJ but not sbt good enough for now
      val _ = findClass(clazz).getOrElse(throw new Exception(s"Compilation Failed: $clazz not compiled from code:\n$code"))
    } catch {
      case e: scala.reflect.internal.MissingRequirementError =>
    }
  }


  protected def classNameForCode(code: String): String = {
    val digest = MessageDigest.getInstance("SHA-1").digest(code.getBytes)
    "sha"+new BigInteger(1, digest).toString(16)
  }

  protected def findClass(className: String): Option[Class[_]] = {
    synchronized {
      classCache.get(className).orElse {
        try {
          val cls = classLoader.loadClass(className)
          classCache(className) = cls
          Some(cls)
        } catch {
          case e: ClassNotFoundException => None
        }
      }
    }
  }
}
