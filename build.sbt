ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.11.12"

lazy val root = (project in file("."))
  .settings(
    name := "solution"
  )

libraryDependencies ++= List("com.softwaremill.sttp.client3" %% "core" % "3.8.16")