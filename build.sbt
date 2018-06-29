scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-unchecked"
)

enablePlugins(JavaAppPackaging)

val modulesDirectory = "modules"

lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.12.4",
  organization := "tlang",
  version := "1.0",
  javacOptions ++= Seq("-encoding", "UTF-8"),
    // Common dependencies used by all modules
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "sourcecode" % "0.1.4",
    "com.github.pathikrit" %% "better-files" % "3.1.0"
  )
)

javaOptions in Universal ++= Seq(
  "-Dfile.encoding=UTF-8"
)

lazy val metaMacroSettings = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-language:experimental.macros",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in(Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in(Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    "org.mockito" % "mockito-core" % "2.9.0" % Test,
    "org.markushauck" %% "mockitoscala" % "0.3.0" % Test
  ),
  parallelExecution in Test := true,
  logBuffered in Test := false
)

// ------------------------------------------------------------------------------------
// --- Modules
// ------------------------------------------------------------------------------------

lazy val macros = (project in file(s"$modulesDirectory/macros"))
  .settings(
    name := "macros",
    commonSettings,
    metaMacroSettings,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.8.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )


lazy val utils = (project in file(s"$modulesDirectory/utils"))
  .settings(
    name := "utils",
    commonSettings,
    metaMacroSettings,
    testSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "jline" % "2.9.0-1", // Used to measure width of terminal
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4" // Used in simple parsers for stack traces etc.
    )
  )
  .dependsOn(macros)


lazy val cafebabe = (project in file(s"$modulesDirectory/cafebabe"))
  .settings(
    name := "cafebabe",
    commonSettings
  )
  .dependsOn(utils)


lazy val compiler = (project in file(s"$modulesDirectory/compiler"))
  .settings(
    name := "compiler",
    mainClass in stage := Some("tlang.compiler.Main"),
    commonSettings,
    metaMacroSettings,
    testSettings,
    libraryDependencies ++= Seq(
      "org.apache.bcel" % "bcel" % "6.2", // Library used for parsing class files
      "org.ow2.asm" % "asm-all" % "5.2" // Used to generate stack map frames
    )
  )
  .dependsOn(macros, utils, cafebabe)

lazy val repl = (project in file(s"$modulesDirectory/repl"))
  .settings(
    name := "repl",
    commonSettings,
    metaMacroSettings,
    testSettings,
    libraryDependencies ++= Seq(
      "com.googlecode.lanterna" % "lanterna" % "3.0.0",
      "org.scalaz" %% "scalaz-core" % "7.2.9", // Uses cord data structure to represent input
      "com.typesafe.akka" %% "akka-actor" % "2.4.17"
    )
  )
  .dependsOn(macros, utils, compiler)

lazy val root = (project in file("."))
  .aggregate(macros, utils, cafebabe, compiler, repl)
  .settings(
    name := "core",
    mainClass in Compile := Some("tlang.compiler.Main"),
    commonSettings,
    metaMacroSettings,
    testSettings,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "sourcecode" % "0.1.4",
      "com.github.pathikrit" %% "better-files" % "3.1.0"
    )
  )
  .dependsOn(macros, utils, cafebabe, compiler, repl)
