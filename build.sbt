
organization := "com.phasmidsoftware"

name := "NumberApps"

version := "1.0.9"

scalaVersion := "3.7.3"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "it" / "scala"

val numberVersion = "1.10.6-SNAPSHOT"
val visitorVersion = "1.6.0"
val scalaTestVersion = "3.2.20"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "number" % numberVersion,
  "com.phasmidsoftware" %% "visitor" % visitorVersion,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.32" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

