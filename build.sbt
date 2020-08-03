name := "metarules"

organization := "com.github.memo33"

version := "0.1.2"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.11.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  "-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.6")

initialCommands in console := """
import metarules._, meta._
import Implicits._, Network._, Flag._, Flags._, RotFlip._, Tile.{CopyTile => %}
implicit val resolve = module.Main.resolve
def transduce(rule: Rule[Tile]): Unit = RuleTransducer(rule) foreach println
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"


resolvers += "stephenjudkins-bintray" at "https://dl.bintray.com/stephenjudkins/maven"

resolvers += "memo33-bintray" at "https://dl.bintray.com/memo33/maven"

libraryDependencies += "com.github.memo33" %% "scdbpf" % "0.1.7"

libraryDependencies += "com.github.memo33" %% "scalaenum" % "0.1.4"
