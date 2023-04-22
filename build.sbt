ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

val scalatestVersion = "3.2.15"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "SnailSort"
  )