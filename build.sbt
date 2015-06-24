name := "koolc"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"uk.com.robust-it" % "cloning" % "1.9.2",
	"org.apache.bcel" % "bcel" % "5.2",
	"org.apache.commons" % "commons-lang3" % "3.4"
)

// src/main/scala only

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

// src/test/scala only

unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil
