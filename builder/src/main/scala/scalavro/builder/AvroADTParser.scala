package scalavro.builder

import java.nio.ByteBuffer

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Json, JsonObject}
import io.circe.syntax._

import scala.reflect.api.Universe
import scalavro.schema.parser.AvscParser._
import scalavro.schema.types.AvscType
import scalavro.schema.types.AvscType._

object AvroADTParser {
  def apply(): AvroADTParser[scala.reflect.runtime.universe.type] = {
    apply(scala.reflect.runtime.universe)
  }
  def apply(universe: Universe): AvroADTParser[universe.type] = {
    new AvroADTParser(universe)
  }
}

class AvroADTParser[U <: Universe](val universe: U) {
  import universe.Flag._
  import universe._

  def buildAllClassesAsStr(record: Record): List[String] =
    buildAllClasses(record).map(t => showCode(t).replace("\n\n", "\n"))

  def buildAllClasses(record: Record): List[universe.PackageDef] = {
    assert(record.namespace.isDefined, "Parent Record Must have a namespace")
    val packages = buildStuff(record.namespace.get, StuffToBuild(List(record), List.empty))
    val result = packages.groupBy(p => fullPackageName(p)).map{
      case (_, pkgs) =>
        val pkgTree = PackageDef.unapply(pkgs.head).map(_._1).get
        val allTree = pkgs.flatMap {
          case PackageDef(_, trees) => trees
        }
        PackageDef(pkgTree, distinctAndSortMembers(allTree))
    }.toList
    result
  }

  private def typeToTypeName(`type`: SimpleAvscType, ns: String): Tree = `type` match {
    case NullType => tq"Null"
    case BoolType => tq"Boolean"
    case IntType => tq"Int"
    case LongType => tq"Long"
    case FloatType => tq"Float"
    case DoubleType => tq"Double"
    case StringType => tq"String"
    case BytesType => tq"java.nio.ByteBuffer"
    case RecordByName(name) => nsToPackage(s"${ns.value}.${name.value}")
  }

  def typeToTypeTree(ns: NonEmptyString, `type`: AvscType): StuffContext[Tree] = `type` match {
    case tn: SimpleAvscType => StuffContext.empty(typeToTypeName(tn, ns.value))
    case r: Record =>
      val namespace = r.namespace.getOrElse(ns)
      val nsTree = nsToPackage(s"${namespace.value}.${r.name.value}")
      StuffContext(nsTree, StuffToBuild(List(r), List.empty))
    case EnumType(name, _, _, _, symbols) =>
      val t = nsToPackage(s"${ns.value}.${name.value}")
      StuffContext(t, StuffToBuild(List.empty, List((ns, name, symbols))))

    case ArrayType(itemType) => typeToTypeTree(ns, itemType).map(t => tq"Array[$t]")
    case MapType(valueType) => typeToTypeTree(ns, valueType).map(t => tq"Map[..${Seq(tq"String", tq"$t")}]")
    case u @ Union(_, _) =>
      val containsNull = u.types.toList.contains(NullType)
      val notNulls = u.types.filter(_ != NullType)
      val baseUnionCtx = buildUnion(ns, notNulls)
      baseUnionCtx.map { baseUnion =>
        if (containsNull) {
          q"Option[$baseUnion]"
        } else {
          baseUnion
        }
      }
    case _: Fixed => StuffContext.empty(tq"Array[Byte]")
  }

  private val cop: TermName = TermName("$colon$plus$colon")

  private def buildUnion(ns: NonEmptyString, types: List[AvscType]): StuffContext[Tree] = {
    types.foldLeft(StuffContext.empty(tq"CNil": Tree)) { case (ctx, next) =>
      for {
        tree <- ctx
        nextTree <- typeToTypeTree(ns, next)
      } yield q"$cop[..${List(nextTree, tree)}]"
    }
  }

  private def fieldToCCParam(ns: NonEmptyString, field: Field): StuffContext[ValDef] = {
    fieldToValDefs(ns, field, Modifiers(MUTABLE | CASEACCESSOR | PARAMACCESSOR))
  }

  private def fieldToConstParam(ns: NonEmptyString, field: Field): StuffContext[ValDef] = {
    val mod = if(field.default.nonEmpty) {
      Modifiers(universe.Flag.PARAM | universe.Flag.PARAMACCESSOR | universe.Flag.DEFAULTPARAM)
    } else {
      Modifiers(universe.Flag.PARAM | universe.Flag.PARAMACCESSOR)
    }
    fieldToValDefs(ns, field, mod)
  }


  private def fieldToValDefs(ns: NonEmptyString, field: Field, modifiers: Modifiers): StuffContext[ValDef] = {
    typeToTypeTree(ns, field.`type`).map { fieldTypeTree =>
      ValDef(
        modifiers,
        TermName(field.name.value),
        fieldTypeTree,
        field.default.fold(EmptyTree){d => defaultToTree(field.`type`)(d)}
      )
    }
  }


  private def defaultToTree(t: AvscType)(default: t.ScalaType): Tree = (t, default) match {
    case (NullType, null) => q"null"
    case (BoolType, d: Boolean) => q"$d"
    case (IntType, d: Int) => q"$d"
    case (LongType, d: Long) => q"$d"
    case (FloatType, d: Float) => q"$d"
    case (DoubleType, d: Double) => q"$d"
    case (StringType, d: String) => q"$d"
    case (BytesType, d: ByteBuffer) => q"java.nio.ByteBuffer.wrap(${d.array()})"
    case (Fixed(_, _, _, _), d: Array[Byte]) => q"$d"
    case (et: EnumType, d: String) => q"${nsToPackage(et.name.value)}.fromString($d)"
    case (at: ArrayType[_], d: List[_]) =>
      val innerMapped = d.map{inD =>
        defaultToTree(at.items)(inD.asInstanceOf[at.items.ScalaType])
      }
      q"Array(..$innerMapped)"
    case (mt: MapType[_], d: Map[_, _]) =>
      val innerMapped = d.map{case (k, inD) =>
        val vt = defaultToTree(mt.values)(inD.asInstanceOf[mt.values.ScalaType])
        q"(${k.asInstanceOf[String]}, $vt)"
      }
      q"Map(..$innerMapped)"
    case (ut: Union[_], d) =>
      val innerT = defaultToTree(ut.head)(d.asInstanceOf[ut.head.ScalaType])
      ut.tail.foldLeft(q"Inl($innerT)"){case (soFar, _) => q"Inr($soFar)"}
    case (r: Record, j: JsonObject) =>  buildClassValue(r, j)
    case (t, v) => throw new MatchError(s"Invalid Type Default Pair ($t, $v)")
  }

  private def buildClassValue(r: Record, jo: JsonObject): Tree = {
    val fTrees = r.
      fields.
      map(f => f.name.value -> jo(f.name.value).getOrElse(throw new Exception(s"Could not find ${f.name} in $jo"))).
      map{case (name, json) => q"${TermName(name)} = ${jsonToTree(json)}"}
    q"${nsToPackage(r.name.value)}(..${fTrees.toList})"
  }

  private def jsonToTree(json: Json): Tree = json match {
    case _ if json.isString => q"${json.asString.get}"
    case _ if json.isNumber =>
      val num = json.asNumber.get
      num.toInt.map(i => q"$i").
        orElse(num.toLong.map(l => q"$l")).
        getOrElse(q"${num.toDouble}")
  }

  private def fieldsToConstrcutor(ns: NonEmptyString, fs: NonEmptyList[Field]): StuffContext[DefDef] = {
    StuffContext.sequence(fs.map(f => fieldToConstParam(ns, f))).map { constParam =>
      DefDef(
        Modifiers(),
        termNames.CONSTRUCTOR,
        List(),
        List(
          constParam
        ),
        TypeTree(),
        Block(List(pendingSuperCall), Literal(Constant(())))
      )
    }
  }

  private def fieldToDefaultValues(ns: NonEmptyString, fs: NonEmptyList[Field]): NonEmptyList[Tree] = {
    fs.map{field =>
      if(field.default.isEmpty) {
        emptyDefaultValue(ns, field.`type`)
      } else {
        defaultToTree(field.`type`)(field.default.get)
      }
    }
  }

  private def emptyDefaultValue(ns: NonEmptyString, t: AvscType): Tree = t match {
    case BoolType => Literal(Constant(false))
    case DoubleType | FloatType | IntType | LongType => Literal(Constant(-1))
    case MapType(values) => q"Map.empty[String, ${typeToTypeTree(ns, values).a}]"
    case _ => Literal(Constant(null))
  }

  private def fieldsToDefaultConstrcutor(ns: NonEmptyString, fs: NonEmptyList[Field]): DefDef = {
    q"def this() = this(..${fieldToDefaultValues(ns, fs).toList})".asInstanceOf[DefDef]
  }

  private def fieldsToTemplate(ns: NonEmptyString, fs: NonEmptyList[Field]): StuffContext[Template] = for {
    valDefs <- StuffContext.sequence(fs.map(f => fieldToCCParam(ns, f)))
    defDefs <- fieldsToConstrcutor(ns, fs)
    defDefaultDefs = fieldsToDefaultConstrcutor(ns, fs)
  } yield tmpl(valDefs ::: List(defDefs, defDefaultDefs))

  private def tmpl(body: List[Tree]): Template = Template(
    List(
      tq"scala.Product",
      tq"scala.Serializable"
    ),
    noSelfType,
    body
  )


  private def distinctAndSortMembers(trees: List[Tree]): List[Tree] = {
    trees.groupBy{
      case cd: ClassDef => cd.name
      case mod: ModuleDef => mod.name
      case imp: Import => imp.toString()
    }.map{case (_, x) => x.head}.toList.sortBy{
      case imp: Import => 0  -> imp.toString()
      case ClassDef(m, n, _, _) => if(m.hasFlag(SEALED)) 1 -> n.toString else 3 -> n.toString
      case mod: ModuleDef => 2 -> mod.name.toString
    }
  }


  private def fullPackageName(pkg: Tree, base: Option[String] = None): String = {
    pkg match {
      case PackageDef(s, _) => fullPackageName(s, base)
      case Select(t, n) => fullPackageName(t, Some(base.fold(n.toString){b => s"$n.$b"}))
      case Ident(TermName(s)) => base.fold(s){b => s"$s.$b"}
    }
  }

  private def buildStuff(ns: NonEmptyString, stuffToBuild: StuffToBuild): List[PackageDef] = {
    val pkgs1 = stuffToBuild.records.flatMap { record =>
      val pkg1Ctx = buildSingleClass(ns, record)
      val pkg2 = buildStuff(record.namespace.getOrElse(ns), pkg1Ctx.stuff)
      pkg1Ctx.a :: pkg2
    }
    val pkgs2 = stuffToBuild.enums.map { case (ns, name, values) => buildSingleEnum(ns, name, values) }
    pkgs1 ++ pkgs2
  }

  private def nsToPackage(ns: String): RefTree = {
    val split = ns.split('.')
    split.tail.foldLeft(Ident(TermName(split.head)): RefTree) { case (tn, s) =>
      Select(tn, TermName(s))
    }
  }


  private def buildSingleClass(ns: NonEmptyString, record: Record): StuffContext[PackageDef] = {
    fieldsToTemplate(record.namespace.getOrElse(ns), record.fields).map { tmpl =>
      PackageDef(
        nsToPackage(record.namespace.getOrElse(ns).value),
        List(
          Import(Select(Ident(TermName("scalavro")), TermName("macros")), List(ImportSelector(TermName("AsIndexedRecord"), 37, TermName("AsIndexedRecord"), 37))),
          ClassDef(
            Modifiers(universe.Flag.CASE, typeNames.EMPTY, List(
              Apply(
                Select(New(Ident(TypeName("AsIndexedRecord"))), termNames.CONSTRUCTOR),
                List(Literal(Constant(record.asJson.noSpaces)))
              )
            )),
            TypeName(record.name.value),
            List.empty,
            tmpl
          )
        )
      )
    }
  }

  private def buildSingleEnum(ns: NonEmptyString, base: NonEmptyString, values: NonEmptyList[NonEmptyString]): PackageDef = {
    val baseName: TypeName = TypeName(base.value)
    val baseClass = q"sealed trait $baseName"
    val objs = baseClass :: values.map{value =>
      q"case object ${TermName(value.value)} extends $baseName"
    }.toList
    q"package ${nsToPackage(ns.value)} {..$objs}".asInstanceOf[PackageDef]
  }
}
