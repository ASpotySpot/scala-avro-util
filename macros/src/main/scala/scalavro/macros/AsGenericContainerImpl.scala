package scalavro.macros


import cats.data.NonEmptyList
import scalavro.util.RefineUtils._
import io.circe.syntax._
import scalavro.schema.parser.AvscParser._
import org.apache.avro.Schema
import scalavro.schema.{Field, Record, StringType}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class AsGenericContainer extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AsGenericContainerImpl.impl
}

object AsGenericContainerImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._


    def modifiedClass(classDecl: ClassDef) = {
      val (className, fields) = try {
        val q"case class $className(..$fields) " = classDecl
        (className, fields)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class")
      }
      val recordRep: Record = fields.length match {
        case 0 => c.abort(c.enclosingPosition, "Cannot annotate classes with no fields")
        case _ =>
          Record(className.toString().refineF, None, None, None, NonEmptyList(Field(
            "field_name".refineF,
            None,
            StringType
          ), Nil)
          )
      }

      println(recordRep)
      val schemaJson: String = recordRep.asJson.noSpaces
      println(schemaJson)
      val params = fields.asInstanceOf[List[ValDef]] map { p => p.duplicate}
      val schema = new Schema.Parser().parse(schemaJson)
      val theDef = s"""override def getSchema(): org.apache.avro.Schema = new Schema.Parser().parse($schemaJson)"""
      val r = c.Expr[Any](
        q"""
        case class $className ( ..$params ) extends org.apache.avro.generic.GenericContainer {
          $theDef
        }
        """
      )
      println(r)
      r
    }

    annottees.map(_.tree).toList match {
      case (classDecl: ClassDef) :: Nil => modifiedClass(classDecl)
      case _ => c.abort(c.enclosingPosition, "Invalid Annottee")
    }
  }
}