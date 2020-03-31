import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"

unmanagedJars in Compile += file("src/main/scala/org/andrewkilpatrick/elmGen/elmGen-0.5.jar")

scalafmtConfig in ThisBuild := baseDirectory.value / ".scalafmt.conf"

lazy val root = (project in file("."))
  .settings(
    name := "spinner",
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-core"                % "2.1.0",  
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "com.beachape"           %% "enumeratum"               % "1.5.15",
      "org.typelevel"          %% "cats-effect"              % "2.1.2"
    )
  )