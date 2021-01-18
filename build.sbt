val dottyVersion = "3.0.0-M3"
val scala213Version = "2.13.4"
val scala212Version = "2.12.13"

ThisBuild / testFrameworks += new TestFramework("munit.Framework")
ThisBuild / version := "0.0.1-pre.1"
ThisBuild / scalaVersion := scala213Version
ThisBuild / organization := "com.martijnhoekstra"


lazy val global = project
  .in(file("."))
  .aggregate(lib, cli)


lazy val lib = project.in(file("semver4s"))
  .settings(
    name := "semver4s",
    crossScalaVersions := List(dottyVersion, scala213Version, scala212Version),
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.0",
      "org.scalameta" %% "munit" % "0.7.20",
      "org.scalameta" %% "munit-scalacheck" % "0.7.20")
  )

lazy val cli = project.in(file("cli"))  
  .settings(
    crossScalaVersions := List(scala213Version, scala212Version),
    name := "semver4s-cli",
    libraryDependencies ++= List(
      "com.monovore" %% "decline" % "1.3.0",
      "com.monovore" %% "decline-effect" % "1.3.0")
  ).dependsOn(lib)