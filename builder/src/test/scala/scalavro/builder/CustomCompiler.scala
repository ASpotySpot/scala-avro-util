package scalavro.builder

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import tools.nsc.io.VirtualDirectory
import scala.reflect.internal.util.AbstractFileClassLoader
import java.security.MessageDigest
import java.math.BigInteger
import collection.mutable

object CompileTest {
  def apply(str: String, classes: List[String]): Unit = (new Compiler).compile(str, classes)
}

class Compiler {

  val target = new VirtualDirectory("(memory)", None)
  val classCache = mutable.Map[String, Class[_]]()

  private val settings = new Settings()
  settings.outputDirs.setSingleOutput(target)
  settings.usejavacp.value = true
  private val global = new Global(settings)
  private lazy val run = new global.Run

  val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  /**Compiles the code as a class into the class loader of this compiler.
    *
    * @param code
    * @return
    */
  def compile(code: String, classes: List[String]) = {
    val sourceFiles = List(new BatchSourceFile("(inline)", code))
    run.compileSources(sourceFiles)
    val missing = classes.map(clazz => clazz  -> findClass(clazz)).filter(_._2.isEmpty).map(_._1)
    if(missing.nonEmpty) {
      throw new Exception(s"Compilation Failed: ${missing.mkString(",")}")
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
