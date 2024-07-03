import sbt.*

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "minesweeper",
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    Compile / unmanagedResourceDirectories += baseDirectory.value / "src" / "main" / "resources"
  )

libraryDependencies += "org.scalafx" %% "scalafx" % "20.0.0-R31"