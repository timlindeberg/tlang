name := """play-scala-starter-example"""

version := "1.0-SNAPSHOT"

lazy val compiler = RootProject(file("../../T-Compiler"))

lazy val backend = (project in file(".")).dependsOn(compiler).enablePlugins(PlayScala)

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

scalaVersion := "2.12.4"

val raptureVersion = "2.0.0-M9"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "com.h2database" % "h2" % "1.4.196",
  "com.propensive" %% "rapture-http" % raptureVersion,
  "com.propensive" %% "rapture-http-json" % raptureVersion,
  "com.propensive" %% "rapture-io" % raptureVersion,
  "com.propensive" %% "rapture-uri" % raptureVersion,
  "com.propensive" %% "rapture-net" % raptureVersion,
  "com.propensive" %% "rapture-json" % raptureVersion,
  "com.propensive" %% "rapture-json-play" % raptureVersion
)
