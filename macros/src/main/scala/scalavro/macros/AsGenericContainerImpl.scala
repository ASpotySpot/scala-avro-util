package scalavro.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class AsGenericContainer(schema: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AsGenericContainerImpl.impl
}

object AsGenericContainerImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val schemaString: String = c.prefix.tree match {
      case q"new $name( ..$params )" if params.length == 1 => params.head.toString.filterNot(_ == '\\').drop(1).dropRight(1)
      case q"new $name( ..$params )" => c.abort(c.enclosingPosition, "Only one annotation parameter is accepted")
      case _ => c.abort(c.enclosingPosition, "No parameters found for annotation")
    }

    def modifiedClass(classDecl: ClassDef) = {
      val (className, fields) = try {
        val q"case class $className(..$fields) " = classDecl
        (className, fields)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class")
      }

      val params = fields.asInstanceOf[List[ValDef]] map { p => p.duplicate}
      val theDef = q"""override def getSchema(): org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse($schemaString)"""
      val r = c.Expr[Any](
        q"""
        case class $className ( ..$params ) extends org.apache.avro.generic.GenericContainer {
          $theDef
        }
        """
      )
      r
    }

    annottees.map(_.tree).toList match {
      case (classDecl: ClassDef) :: Nil => modifiedClass(classDecl)
      case _ => c.abort(c.enclosingPosition, "Invalid Annottee")
    }
  }
}