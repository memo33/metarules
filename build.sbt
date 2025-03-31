name := "metarules"

organization := "io.github.memo33"

version := "0.7.0"

ThisBuild / versionScheme := Some("early-semver")

description := "A RUL2 meta language for SC4"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "3.6.4"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  // "-opt-warnings:at-inline-failed-summary",
  // "-opt:l:inline", "-opt-inline-from:<sources>",
  "-encoding", "UTF-8",
  "-release:8")

javacOptions ++= Seq("--release", "8")

console / initialCommands := """
import io.github.memo33.metarules._, io.github.memo33.metarules.meta._
import internal.DummyNetwork._, Implicits._, Flags._, RotFlip._, Rule.{CopyTile => %}, group.SymGroup._
"""

publishTo := sonatypePublishToBundle.value

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"

useGpgPinentry := true  // see https://github.com/sbt/sbt-pgp/issues/178 or https://github.com/microsoft/WSL/issues/4029#issuecomment-491547314

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test

libraryDependencies += "io.github.memo33" %% "scalaenum" % "0.2.0" cross CrossVersion.for3Use2_13

libraryDependencies += "io.github.memo33" %% "scdbpf" % "0.2.0" cross CrossVersion.for3Use2_13
