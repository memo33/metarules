name := "metarules"

organization := "com.github.memo33"

version := "0.5.1-SNAPSHOT"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.13.10"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  // "-opt-warnings:at-inline-failed-summary",
  "-opt:l:inline",
  // "-opt-inline-from:**",  // leads to compiler errors with scala-2.13
  "-encoding", "UTF-8",
  "-release:8")

javacOptions ++= Seq("--release", "8")

console / initialCommands := """
import metarules._, metarules.meta._
import internal.DummyNetwork._, Implicits._, Flags._, RotFlip._, Rule.{CopyTile => %}, group.SymGroup._
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "io.github.memo33" %% "scalaenum" % "0.2.0"

libraryDependencies += "io.github.memo33" %% "scdbpf" % "0.2.0"
