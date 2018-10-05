// See LICENSE for license details.

name := "fft"

organization := "edu.berkeley.cs"

version := "1.0"

scalaVersion := "2.12.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "rocket-dsptools" -> "1.2-SNAPSHOT",
  "tapeout" -> "0.1-SNAPSHOT"
)

libraryDependencies ++= Seq("rocket-dsptools").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) }

libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5",
  "org.scalacheck" %% "scalacheck" % "1.14.0")

lazy val TravisTest = config("travis") extend(Test)

lazy val fft = (project in file(".")).
  configs(TravisTest).
  settings(inConfig(TravisTest)(Defaults.testTasks): _*)

testOptions in TravisTest += Tests.Argument(TestFrameworks.ScalaTest, "-l", "edu.berkeley.tags.LocalTest", "-eF")
