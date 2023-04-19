name := "metarules"

organization := "com.github.memo33"

version := "0.5.1-SNAPSHOT"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.11.12"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  "-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

console / initialCommands := """
import metarules._, metarules.meta._
import internal.DummyNetwork._, Implicits._, Flags._, RotFlip._, Rule.{CopyTile => %}, group.SymGroup._
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "io.github.memo33" %% "scalaenum" % "0.2.0"

libraryDependencies += "io.github.memo33" %% "scdbpf" % "0.2.0"
