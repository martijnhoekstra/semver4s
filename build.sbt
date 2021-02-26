val dottyVersion    = "3.0.0-RC1"
val scala213Version = "2.13.5"
val scala212Version = "2.12.13"

import xerial.sbt.Sonatype._

ThisBuild / testFrameworks += new TestFramework("munit.Framework")
ThisBuild / scalaVersion := scala213Version
ThisBuild / organization := "com.heroestools"
ThisBuild / licenses := Seq("LGPLv3" -> url("https://www.gnu.org/licenses/lgpl-3.0.en.html"))
ThisBuild / developers := List(
  Developer(
    id = "Martijn",
    name = "Martijn Hoekstra",
    email = "martijnhoekstra@gmail.com",
    url = url("https://www.github.com")
  )
)
ThisBuild / parallelExecution := false

//a subproject "semver4s" gets automatically created
//and aggregates all subprojects.
//I don't think you can disable that project, only rename it
//lazy val global = project
//  .in(file("."))
//  .aggregate(lib, cli)
//  .settings(
//    publish / skip := true
//  )

lazy val lib = projectMatrix
  .in(file("semver4s"))
  .settings(
    name := "semver4s",
    version := "0.3.0",
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-parse"       % "0.3.1",
      "org.scalameta" %%% "munit"            % "0.7.22" % "test",
      "org.scalameta" %%% "munit-scalacheck" % "0.7.22" % "test"
    ),
    libraryDependencies ++= List(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    )
      .filter(_ => scalaVersion.value.startsWith("2")),
    publishTo := sonatypePublishToBundle.value,
    sonatypeProjectHosting := Some(
      GitHubHosting("martijnhoekstra", "semver4s", "martijnhoekstra@gmail.com")
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

  )
  .jvmPlatform(scalaVersions = List(dottyVersion, scala212Version, scala213Version))
  .jsPlatform(scalaVersions = List(dottyVersion, scala212Version, scala213Version))

lazy val cli = projectMatrix
  .in(file("cli"))
  .settings(
    name := "semver4s-cli",
    version := "1.0.2",
    libraryDependencies ++= List(
      "com.monovore" %%% "decline"        % "1.3.0",
      "com.monovore" %%% "decline-effect" % "1.3.0"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(lib)
  .jvmPlatform(scalaVersions = List(scala212Version, scala213Version))
  .jsPlatform(scalaVersions = List(scala212Version, scala213Version))


lazy val npmfacade =  projectMatrix.in(file("npmfacade"))
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    testFrameworks += new TestFramework("munit.Framework"),
    name := "npmFacade",
    version := "0.0.1",
    npmDependencies in Test += "semver" -> "7.3.4",
    libraryDependencies ++= List(
      "org.scalameta" %%% "munit"            % "0.7.22" % "test",
      "org.scalameta" %%% "munit-scalacheck" % "0.7.22" % "test"),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .jsPlatform(scalaVersions = List(dottyVersion, scala213Version))
  .dependsOn(lib % "compile->compile;test->test")
  