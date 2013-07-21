import sbt._
import Keys._

object Dependencies {

  object Resolvers {
    val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"

    val commons = Seq(sonatype)

  }

  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.4"

}