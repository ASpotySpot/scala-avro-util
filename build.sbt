lazy val commonSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  organization := "scalavro",
  scalaVersion := "2.12.4",
  resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
  resolvers += Resolver.jcenterRepo,
  scalacOptions ++= scalacFlags,
  sourceGenerators in Compile += (avroScalaGenerate in Compile).taskValue
)
lazy val sharedDeps = Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "eu.timepit" %% "refined" % "0.9.0",
  "eu.timepit" %% "refined-scalacheck" % "0.9.2" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

lazy val codec = (project in file("codec")).settings(
  commonSettings,
  name := "scalavro-codec",
  libraryDependencies ++= sharedDeps ++ Seq(
    "org.scodec" %% "scodec-bits" % "1.1.5",
    "org.scodec" %% "scodec-core" % "1.10.3",
    "org.apache.avro" % "avro" % "1.8.2" % Test
  )
)
lazy val schema = (project in file("schema")).settings(
  commonSettings,
  name := "scalavro-schema",
  libraryDependencies ++= sharedDeps ++ Seq(
    "io.circe" %% "circe-core" % "0.9.3",
    "io.circe" %% "circe-generic" % "0.9.3",
    "io.circe" %% "circe-parser" % "0.9.3",
    "org.apache.avro" % "avro" % "1.8.2" % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.0" % Test
  )
)

lazy val builder = (project in file("builder"))
  .dependsOn(schema)
  .settings(
    commonSettings,
    name := "scalavro-class-builder",
    libraryDependencies ++= sharedDeps ++ Seq(
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.0" % Test
    )
  )

lazy val macros = (project in file("macros"))
  .dependsOn(builder)
  .settings(
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.4",
    libraryDependencies += "org.apache.avro" % "avro" % "1.8.2",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    ),
    name := "scalavro-macros"
  )

lazy val `macros-test` = (project in file("macros-test"))
  .dependsOn(macros)
  .settings(
    commonSettings,
    name := "scalavro-macros-test",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

lazy val plugin = (project in file("plugin"))
  .dependsOn(schema, builder)
  .settings(
    name := "scalavro-sbt",
    sbtPlugin := true,
    test in assembly := {},
//    logLevel in assembly := Level.Debug,
    assemblyOption in assembly := (assemblyOption in assembly).value
      .copy(includeScala = false),
    assemblyShadeRules in assembly := Seq(
      ShadeRule.rename("io.circe.**" -> "shaded.circe.@1").inAll,
      ShadeRule.rename("jawn.**" -> "shaded.jawn.@1").inAll
    ),
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.withClassifier(Some("assembly"))
    },
    addArtifact(artifact in (Compile, assembly), assembly)
  )

lazy val root = (project in file(".")).aggregate(`macros-test`,
                                                 macros,
                                                 codec,
                                                 schema,
                                                 builder,
                                                 plugin)

lazy val scalacFlags = Seq(
//  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
//  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xfuture", // Turn on future language features.
//  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
//  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
//  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
//  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
//  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
//  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
//  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
//  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
//  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
//  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
//  "-Xlint:option-implicit", // Option.apply used implicit view.
//  "-Xlint:package-object-classes", // Class or object defined in package object.
//  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
//  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
//  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
//  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
//  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
//  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
//  "-Ywarn-unused:params", // Warn if a value parameter is unused.
//  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
  "-Ypatmat-exhaust-depth",
  "40"
)
