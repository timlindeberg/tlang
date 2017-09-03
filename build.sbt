lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.12.1",
  organization := "com.tcompiler",
  version := "1.0",
  javacOptions ++= Seq("-encoding", "UTF-8")
)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-unchecked",
  "–encoding UTF8"
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-language:experimental.macros",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in(Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in(Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val macros = project
  .settings(
    commonSettings,
    metaMacroSettings,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.7.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )


lazy val core = project
  .settings(
    commonSettings,
    metaMacroSettings,
    name := "T-Compiler",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
      "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test,
      "com.google.code.findbugs" % "bcel-findbugs" % "6.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.ow2.asm" % "asm-all" % "5.1",
      "org.graphstream" % "gs-core" % "1.1.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta3",
      "org.scala-lang" % "jline" % "2.9.0-1",
      "org.scalaz" %% "scalaz-core" % "7.2.9",
      "com.typesafe.akka" %% "akka-actor" % "2.4.17",
      "com.github.pathikrit" %% "better-files" % "3.1.0"
    ),
    parallelExecution in Test := true,
    logBuffered in Test := false
  )
  .dependsOn(macros)

