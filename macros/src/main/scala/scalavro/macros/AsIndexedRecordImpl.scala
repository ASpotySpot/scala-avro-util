package scalavro.macros

import eu.timepit.refined.collection.NonEmpty
import io.circe.parser._
import scalavro.schema.parser.AvscParser._
import scalavro.builder.AvroADTParser
import scalavro.schema.types.AvscType._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import eu.timepit.refined.refineV

class AsIndexedRecord(schema: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AsIndexedRecordImpl.impl
}

object AsIndexedRecordImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val adtBuilder = AvroADTParser(c.universe)
    import c.universe._
    val packageName = refineV[NonEmpty](c.enclosingPackage.toString().drop(8).takeWhile(!_.isWhitespace)) match {
      case Right(value) => value
      case Left(_) => c.abort(c.enclosingPosition, "Could not extract package name")
    }

    val schemaString: String = c.prefix.tree match {
      case q"new $name( ..$params )" if params.length == 1 => params.head.toString.filterNot(_ == '\\').drop(1).dropRight(1)
      case q"new $name( ..$params )" => c.abort(c.enclosingPosition, "Only one annotation parameter is accepted")
      case _ => c.abort(c.enclosingPosition, "No parameters found for annotation")
    }
    val record: Record = decode[Record](schemaString) match {
      case Right(record) => record
      case Left(err) => c.abort(c.enclosingPosition, s"Schema Parse Error: $schemaString: $err")
    }

    def modifiedClass(classDecl: ClassDef) = {
      val (className, fields) = try {
        val q"case class $className(..$fields) " = classDecl
        (className, fields)
      } catch {
        case _: MatchError =>
          try {
            val q"case class $className(..$fields) {$body}" = classDecl
            (className, fields)
          } catch {
            case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class")
          }
      }

      val params = fields.asInstanceOf[List[ValDef]] map { p => p.duplicate}
      val r = c.Expr[Any](
        q"""
        case class $className ( ..$params ) extends org.apache.avro.generic.IndexedRecord {
          import scalavro.schema.parser.AvscParser._
          override def getSchema(): org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse($schemaString)
          val record: scalavro.schema.types.AvscType.Record = io.circe.parser.decode[scalavro.schema.types.AvscType.Record]($schemaString).right.get
          override def get(i: Int): AnyRef = productElement(i).asInstanceOf[AnyRef]
          override def put(i: Int, v: Any): Unit = i match {
            case ..${record.fields.toList.zipWithIndex.map { case (f, i) => cq"""$i => ${TermName(f.name.toString)} = v.asInstanceOf[${adtBuilder.typeToTypeTree(record.namespace.getOrElse(packageName), f.`type`).a}]""" }
        }
        }
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