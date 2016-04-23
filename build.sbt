
crossPaths := false

lazy val commonSettings = Seq(
	scalaVersion := "2.11.8",
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
			"com.novocode" % "junit-interface" % "0.8" % "test->default",
			"org.scalatest" %% "scalatest" % "2.2.4" % "test",
			"junit" % "junit" % "4.11" % "test",
			"uk.com.robust-it" % "cloning" % "1.9.2",
			"org.apache.bcel" % "bcel" % "5.2",
			"org.apache.commons" % "commons-lang3" % "3.4",
			"org.scala-lang" % "scala-compiler" % "2.11.8",
			"org.backuity" %% "ansi-interpolator" % "1.1" % "provided",
			"org.ow2.asm" % "asm-all" % "5.1"
		),

		scalaSource in Compile := baseDirectory.value / "src/scala",
		scalaSource in Test := baseDirectory.value / "test/scala"
	)

lazy val macroSub = (project in file("macro")).
	settings(commonSettings: _*).
	settings(
		libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
	)
