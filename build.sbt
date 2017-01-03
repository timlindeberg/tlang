
crossPaths := false

lazy val commonSettings = Seq(
	scalaVersion := "2.12.1",
	organization := "com.tcompiler",
	version := "1.0"
)

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked",
	"–encoding UTF8"
)

lazy val core = (project in file(".")).
	dependsOn(macroSub).
	settings(commonSettings: _*).
	settings(
		name := "T Compiler",
		libraryDependencies ++= Seq(
			"org.scalatest" %% "scalatest" % "3.0.1" % "test",
			"com.google.code.findbugs" % "bcel-findbugs" % "6.0",
			"org.apache.commons" % "commons-lang3" % "3.4",
			"org.scala-lang" % "scala-compiler" % scalaVersion.value,
			"org.ow2.asm" % "asm-all" % "5.1",
			"org.graphstream" % "gs-core" % "1.1.1",
			"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
),

		scalaSource in Compile := baseDirectory.value / "src/scala",
		scalaSource in Test := baseDirectory.value / "test/scala",
		parallelExecution in Test := false
	)

lazy val macroSub = (project in file("macro")).
	settings(commonSettings: _*).
	settings(
		libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
	)
