package scalavro.macros

import cats.data.NonEmptyList
import scalavro.util.RefineUtils._
import io.circe.syntax._
import io.circe.generic._
import io.circe.parser._
import scalavro.schema.parser.AvscParser._
import org.apache.avro.Schema
import org.apache.avro.generic.IndexedRecord
import scalavro.schema._
import scalavro.schema.parser.AvscParser
import scalavro.schema.types.AvscType
import scalavro.schema.types.AvscType._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.Failure

class AsIndexedRecord(schema: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AsIndexedRecordImpl.impl
}

object AsIndexedRecordImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    type CT = c.universe.Tree
    import c.universe._

    val schemaString: String = c.prefix.tree match {
      case q"new $name( ..$params )" if params.length == 1 => params.head.toString.filterNot(_ == '\\').drop(1).dropRight(1)
      case q"new $name( ..$params )" => c.abort(c.enclosingPosition, "Only one annotation parameter is accepted")
      case _ => c.abort(c.enclosingPosition, "No parameters found for annotation")
    }
    val record: Record = decode[Record](schemaString) match {
      case Right(record) => record
      case Left(err) => c.abort(c.enclosingPosition, s"Schema Parse Error: $schemaString: $err")
    }


    val cop: TypeName = TypeName("$colon$plus$colon")
    def buildUnion(types: List[AvscType]): Tree = {
      if (types.length == 1) {
        typeToTree(types.head)
      } else {
        types.foldLeft(Ident(TypeName("CNil")): Tree) { case (tree, next) =>
          AppliedTypeTree(Ident(cop), List(typeToTree(next), tree))
        }
      }
    }
    def wrapInOption(tree: Tree): Tree = {
      AppliedTypeTree(
        Ident(TypeName("Option")),
        List(tree)
      )
    }
    def recbyNameToTypeName(ns: String): Tree = {
      val split = ns.split('.')
      split.tail.foldLeft(Ident(TypeName(split.head)): RefTree) {case (tn, s) =>
        Select(tn, TypeName(s))
      }
    }
    def typeToTree(avscType: AvscType): c.universe.Tree = avscType match {
      case NullType => Ident(TypeName("Null"))
      case BoolType => Ident(TypeName("Boolean"))
      case IntType => Ident(TypeName("Int"))
      case LongType => Ident(TypeName("Long"))
      case FloatType => Ident(TypeName("Float"))
      case DoubleType => Ident(TypeName("Double"))
      case BytesType => Select(Select(Ident(TermName("java")), TermName("nio")), TypeName("ByteBuffer"))
      case StringType => Ident(TypeName("String"))
      case u @ Union(_, _) =>
        val containsNull = u.types.toList.contains(NullType)
        val notNulls = u.types.filter(_ != NullType)
        val baseUnion = buildUnion(notNulls)
        if (containsNull) {
          wrapInOption(baseUnion)
        } else {
          baseUnion
        }
      case ArrayType(a) => AppliedTypeTree(
        Ident(TypeName("Array")),
        List(typeToTree(a))
      )
      case MapType(a) => AppliedTypeTree(
        Ident(TypeName("Map")),
        List(typeToTree(a))
      )
      case EnumType(name, _, _, _, symbols) =>
        recbyNameToTypeName(s"${name.value}")
      case Fixed(_, _, _, _) => q"Array[Byte]"
      case r: Record => recbyNameToTypeName(s"${r.name.value}")
      case RecordByName(name) => Ident(TypeName(name.value))
    }


    val importsDef: CT = q"""import scalavro.schema.parser.AvscParser._"""
    val schemaDef: CT = q"""override def getSchema(): org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse($schemaString)"""
    val recordDef: CT = q"""val record: scalavro.schema.types.AvscType.Record = io.circe.parser.decode[scalavro.schema.types.AvscType.Record]($schemaString).right.get"""
    val getDef: CT = q"""override def get(i: Int): AnyRef = productElement(i).asInstanceOf[AnyRef]"""
    val putDef: CT =
      q"""override def put(i: Int, v: Any): Unit = i match {
         case ..${
        record.
          fields.toList.zipWithIndex.
          map { case (f, i) => cq"""$i => ${TermName(f.name.toString)} = v.asInstanceOf[${typeToTree(f.`type`)}]""" }
        }}
       """

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
          $importsDef
          $schemaDef
          $recordDef
          $getDef
          $putDef
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