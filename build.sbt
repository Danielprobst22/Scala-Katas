ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

val zioPreludeVersion = "1.0.0-RC19"
val oxVersion = "0.3.1"
val scalatestVersion = "3.2.15"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-prelude" % zioPreludeVersion,
  "com.softwaremill.ox" %% "core" % oxVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % "test",
)

lazy val root = (project in file("."))
  .settings(
    name := "Katas"
  )