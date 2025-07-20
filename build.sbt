
organization := "com.phasmidsoftware"

name := "NumberApps"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "number" % "1.2.6-SNAPSHOT",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.18" % "test",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

