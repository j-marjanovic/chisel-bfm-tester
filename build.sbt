
ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.4.0-SNAPSHOT"
ThisBuild / organization     := "io.j-marjanovic"

lazy val root = (project in file("."))
  .settings(
    name := "chisel-bfmtester",
    libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.3-SNAPSHOT",
    libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.2-SNAPSHOT"
  )

def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
// TODO scalacOptions in Compile ++= Seq("-doc-root-content", "rootdoc.txt")

javacOptions ++= javacOptionsVersion(scalaVersion.value)
