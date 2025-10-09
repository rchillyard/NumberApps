
organization := "com.phasmidsoftware"

name := "NumberApps"

version := "1.0.2"

scalaVersion := "3.7.3"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" % "number_2.13" % "1.2.10",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test" // This is used for testing Rational
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

