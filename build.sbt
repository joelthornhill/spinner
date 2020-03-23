import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

unmanagedJars in Compile += file(Path.userHome+"/OVO/ElmGen/export/elmGen-0.5.jar")

lazy val root = (project in file("."))
  .settings(
    name := "spinner",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.0",  
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "com.beachape" %% "enumeratum" % "1.5.15"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
