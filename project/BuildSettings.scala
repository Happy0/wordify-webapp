import sbt._
import Keys._

object BuildSettings {

  import Dependencies._

  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.lichess",
    scalaVersion := "2.10.2",
    resolvers ++= Dependencies.Resolvers.commons,
    parallelExecution in Test := false,
    scalacOptions := compilerOptions,
    sources in doc in Compile := List())

  val compilerOptions = Seq("-deprecation", "-unchecked", "-feature", "-language:_")

  def defaultDeps = Seq(scalaz)

  def project(name: String, deps: Seq[sbt.ClasspathDep[sbt.ProjectReference]] = Seq.empty) =
    Project(
      name,
      file("modules/" + name),
      dependencies = deps,
      settings = Seq(
        version := "1.0",
        libraryDependencies := defaultDeps) ++ buildSettings)

  def projectToRef(p: Project): ProjectReference = LocalProject(p.id)
  def classpathDependency(p: ProjectReference): ClasspathDependency = new ClasspathDependency(p, None)

}