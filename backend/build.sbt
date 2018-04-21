name := """play-scala-starter-example"""

version := "1.0-SNAPSHOT"

lazy val compiler = RootProject(file("../../T-Compiler"))

lazy val backend = (project in file(".")).dependsOn(compiler).enablePlugins(PlayScala)

resolvers += Resolver.sonatypeRepo("snapshots")
scalaVersion := "2.12.4"


libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "com.h2database" % "h2" % "1.4.196",
)
