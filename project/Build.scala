import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  import BuildSettings._
  import Dependencies._

  val appName = "liscrabble"
  val appVersion = "1.0-SNAPSHOT"

  lazy val modules = Seq(scrabble)

  lazy val scrabble = project("scalascrabble")

  val main = play.Project(appName, appVersion).settings(
    libraryDependencies ++= Seq(scalaz)) dependsOn scrabble

}
