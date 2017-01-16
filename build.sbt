// See LICENSE for license details.

name := "fft"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "dsptools" -> "1.0",
  "dspblocks" -> "1.0",
  "chisel3" -> "3.1-SNAPSHOT",
  "chisel-iotesters" -> "1.2-SNAPSHOT"
  )

libraryDependencies ++= Seq("dsptools", "dspblocks", "chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) }

libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5",
  "org.scalacheck" %% "scalacheck" % "1.12.4")

lazy val TravisTest = config("travis") extend(Test)

lazy val fft = (project in file(".")).
  configs(TravisTest).
  settings(inConfig(TravisTest)(Defaults.testTasks): _*)

testOptions in TravisTest += Tests.Argument(TestFrameworks.ScalaTest, "-l", "edu.berkeley.tags.LocalTest")
