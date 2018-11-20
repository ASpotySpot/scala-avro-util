package scalavro.schema.builder

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import scalavro.schema._

object AvroADTParser {
  val universe: scala.reflect.runtime.universe.type =
    scala.reflect.runtime.universe

  import universe.Flag._
  import universe._

  private case class StuffContext[A](a: A, stuff: StuffToBuild) {
    def addStuff(newStuff: StuffToBuild): StuffContext[A] = new StuffContext[A](a, newStuff |+| stuff)
    def map[B](f: A => B): StuffContext[B] = new StuffContext[B](f(a), stuff)
    def flatMap[B](f: A => StuffContext[B]): StuffContext[B] = f(a).addStuff(stuff)
  }

  private object StuffContext {
    def empty[A](a: A): StuffContext[A] = new StuffContext[A](a, StuffToBuild.empty)

    def sequence[A](ls: NonEmptyList[StuffContext[A]]): StuffContext[List[A]] = {
      ls.foldLeft(StuffContext.empty(List.empty[A])) { (ctxAs, ctxA) =>
        for {
          as <- ctxAs
          a <- ctxA
        } yield a :: as
      }
    }
  }

  private case class StuffToBuild(records: List[Record], enums: List[(NonEmptyString, NonEmptyString, NonEmptyList[NonEmptyString])]) {
    def |+|(newStuff: StuffToBuild): StuffToBuild = StuffToBuild(records ++ newStuff.records, enums ++ newStuff.enums)
  }

  private object StuffToBuild {
    def empty: StuffToBuild = StuffToBuild(List.empty, List.empty)

    def flatten(stuffs: List[StuffToBuild]): StuffToBuild = stuffs.reduceOption(_ |+| _).getOrElse(StuffToBuild.empty)
  }

//  private def nsToRefTree(ns: String): RefTree = {
//    val split = ns.split('.')
//    val init = RefTree(q"", TermName(split.head))
//    split.tail.foldLeft(init) { case (t, qual) => RefTree(t, TermName(qual)) }
//  }

  private def typeToTypeName(`type`: SimpleAvscType): TypeName = `type` match {
    case NullType => TypeName("Null")
    case BoolType => TypeName("Boolean")
    case IntType => TypeName("Int")
    case LongType => TypeName("Long")
    case FloatType => TypeName("Float")
    case DoubleType => TypeName("Double")
    case StringType => TypeName("String")
    case BytesType => ???
    case RecordByName(name: String) => TypeName(name)
  }

  private def typeToTypeTree(ns: NonEmptyString, `type`: AvscType): StuffContext[Tree] = `type` match {
    case tn: SimpleAvscType => StuffContext.empty(Ident(typeToTypeName(tn)))
    case r: Record => StuffContext(Ident(TypeName(r.name.value)), StuffToBuild(List(r), List.empty))
    case EnumType(name, _, _, _, symbols) => StuffContext(Ident(TypeName(name.value)), StuffToBuild(List.empty, List((ns, name, symbols))))

    case ArrayType(itemType) => for {
      typeTree <- typeToTypeTree(ns, itemType)
    } yield AppliedTypeTree(
      Ident(TypeName("Array")),
      List(typeTree)
    )
    case MapType(_) =>
      ???
    case Union(types) =>
      val containsNull = types.toList.contains(NullType)
      val notNulls = types.filter(_ != NullType)
      val baseUnionCtx = buildUnion(ns, notNulls)
      baseUnionCtx.map { baseUnion =>
        if (containsNull) {
          wrapInOption(baseUnion)
        } else {
          baseUnion
        }
      }
    case _: Fixed => ???
  }


  private def wrapInOption(tree: Tree): Tree = {
    AppliedTypeTree(
      Ident(TypeName("Option")),
      List(tree)
    )
  }

  private val cop: TypeName = TypeName("$colon$plus$colon")

  private def buildUnion(ns: NonEmptyString, types: List[AvscType]): StuffContext[Tree] = {
    if (types.length == 1) {
      typeToTypeTree(ns, types.head)
    } else {
      types.foldLeft(StuffContext.empty(Ident(TypeName("CNil")): Tree)) { case (ctx, next) =>
        for {
          tree <- ctx
          nextTree <- typeToTypeTree(ns, next)
        } yield AppliedTypeTree(Ident(cop), List(nextTree, tree))
      }
    }
  }

  private def fieldToCCParam(ns: NonEmptyString, field: Field): StuffContext[ValDef] =
    fieldToValDefs(ns, field, Modifiers(MUTABLE | CASEACCESSOR | PARAMACCESSOR))

  private def fieldToConstParam(ns: NonEmptyString, field: Field): StuffContext[ValDef] =
    fieldToValDefs(ns, field, Modifiers(universe.Flag.PARAM | universe.Flag.PARAMACCESSOR))

  private def fieldToValDefs(ns: NonEmptyString, field: Field, modifiers: Modifiers): StuffContext[ValDef] = {
    typeToTypeTree(ns, field.`type`).map { fieldTypeTree =>
      ValDef(
        modifiers,
        TermName(field.name.value),
        fieldTypeTree,
        EmptyTree
      )
    }
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

  private def fieldsToTemplate(ns: NonEmptyString, fs: NonEmptyList[Field]): StuffContext[Template] = for {
    valDefs <- StuffContext.sequence(fs.map(f => fieldToCCParam(ns, f)))
    defDefs <- fieldsToConstrcutor(ns, fs)
  } yield tmpl(valDefs ::: List(defDefs))

  private def tmpl(body: List[Tree]): Template = Template(
    List(
      Select(
        Ident(TermName("scala")),
        TypeName("Product")
      ),
      Select(
        Ident(TermName("scala")),
        TypeName("Serializable")
      )
    ),
    noSelfType,
    body
  )

  def buildAllClassesAsStr(record: Record): List[String] =
    buildAllClasses(record).map(t => showCode(t).replace("\n\n", "\n"))

  def buildAllClasses(record: Record): List[PackageDef] = {
    assert(record.namespace.isDefined, "Parent Record Must have a namespace")
    val packages = buildStuff(record.namespace.get, StuffToBuild(List(record), List.empty))
    packages.groupBy(p => fullPackageName(p)).map{
      case (_, pkgs) =>
        val pkgTree = PackageDef.unapply(pkgs.head).map(_._1).get
        val allTree = pkgs.flatMap {
          case PackageDef(_, trees) => trees
        }
        PackageDef(pkgTree, distinctAndSortMembers(allTree))
    }.toList
  }
  private def distinctAndSortMembers(trees: List[Tree]): List[Tree] = {
    trees.groupBy{
      case cd: ClassDef => cd.name
      case mod: ModuleDef => mod.name
    }.map{case (_, x) => x.head}.toList.sortBy{
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

  private def nsToPackage(ns: NonEmptyString): RefTree = {
    val split = ns.value.split('.')
    split.tail.foldLeft(Ident(TermName(split.head)): RefTree) { case (tn, s) =>
      Select(tn, TermName(s))
    }
  }

  private def buildSingleClass(ns: NonEmptyString, record: Record): StuffContext[PackageDef] = {
    fieldsToTemplate(record.namespace.getOrElse(ns), record.fields).map { tmpl =>
      PackageDef(
        nsToPackage(record.namespace.getOrElse(ns)),
        List(
          ClassDef(
            Modifiers(universe.Flag.CASE, typeNames.EMPTY),
            TypeName(record.name.value),
            List.empty,
            tmpl
          )
        )
      )
    }
  }

  private def buildSingleEnum(ns: NonEmptyString, base: NonEmptyString, values: NonEmptyList[NonEmptyString]): PackageDef = {
    val baseClass = ClassDef(
      Modifiers(ABSTRACT | INTERFACE | SEALED | DEFAULTPARAM),
      TypeName(base.value),
      List.empty,
      Template(
        List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
        noSelfType,
        List.empty
      )
    )
    val caseObjects = values.map { value =>
      ModuleDef(
        Modifiers(CASE),
        TermName(value.value),
        Template(
          List(
            Ident(TypeName(base.value)),
            Select(Ident(TermName("scala")), TypeName("Product")),
            Select(Ident(TermName("scala")), TypeName("Serializable"))
          ),
          noSelfType,
          List(DefDef(
            Modifiers(),
            termNames.CONSTRUCTOR,
            List.empty,
            List(List.empty),
            TypeTree(),
            Block(List(pendingSuperCall), Literal(Constant(()))
            )
          ))
        )
      )
    }
    PackageDef(
      nsToPackage(ns),
      (baseClass :: caseObjects).toList
    )
  }
}
