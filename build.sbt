import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name := "lang3",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.1.3",
    libraryDependencies += scalaTest % Test
  )
