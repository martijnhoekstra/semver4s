val dottyVersion = "3.0.0-M3"
val scala213Version = "2.13.4"
val scala212Version = "2.12.13"

import xerial.sbt.Sonatype._

ThisBuild / testFrameworks += new TestFramework("munit.Framework")
ThisBuild / scalaVersion := scala213Version
ThisBuild / organization := "com.heroestools"
ThisBuild / licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers := List(
  Developer(id="Martijn", name="Martijn Hoekstra", email="martijnhoekstra@gmail.com", url=url("https://www.github.com"))
)


lazy val global = project
  .in(file("."))
  .aggregate(lib, cli)
  .settings(
    publish / skip := true
  )


lazy val lib = project.in(file("semver4s"))
  .settings(
    name := "semver4s",
    version := "0.1.0",
    crossScalaVersions := List(dottyVersion, scala213Version, scala212Version),
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.0",
      "org.scalameta" %% "munit" % "0.7.20",
      "org.scalameta" %% "munit-scalacheck" % "0.7.20"),
    publishTo := sonatypePublishToBundle.value,
    sonatypeProjectHosting := Some(GitHubHosting("martijnhoekstra", "semver4s", "martijnhoekstra@gmail.com"))
  )

lazy val cli = project.in(file("cli"))  
  .settings(
    crossScalaVersions := List(scala213Version, scala212Version),
    name := "semver4s-cli",
    version := "1.0.0",
    libraryDependencies ++= List(
      "com.monovore" %% "decline" % "1.3.0",
      "com.monovore" %% "decline-effect" % "1.3.0")
  ).dependsOn(lib)