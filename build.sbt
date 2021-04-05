val dottyVersion    = "3.0.0-RC1"
val scala213Version = "2.13.5"
val scala212Version = "2.12.13"

import xerial.sbt.Sonatype._

ThisBuild / testFrameworks += new TestFramework("munit.Framework")
ThisBuild / scalaVersion := scala213Version
ThisBuild / versionScheme := Some("early-semver")
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

//attempt not to grind to a halt
ThisBuild / concurrentRestrictions ++= {
  if (sys.env.contains("CI")) List(Tags.limitAll(1)) else Nil
}

val batchModeOnCI =
  if (sys.env.contains("CI")) List(scalaJSLinkerConfig ~= {
    _.withBatchMode(true)
  })
  else Nil

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
    version := "0.4.0",
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-parse"       % "0.3.2",
      "org.scalameta" %%% "munit"            % "0.7.23" % "test",
      "org.scalameta" %%% "munit-scalacheck" % "0.7.23" % "test"
    ),
    libraryDependencies ++= List(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    )
      .filter(_ => scalaVersion.value.startsWith("2")),
    scalacOptions --= List("-Xfatal-warnings").filter(_ => scalaVersion.value.startsWith("3")),
    publishTo := sonatypePublishToBundle.value,
    sonatypeProjectHosting := Some(
      GitHubHosting("martijnhoekstra", "semver4s", "martijnhoekstra@gmail.com")
    )
  )
  .jvmPlatform(scalaVersions = List(dottyVersion, scala212Version, scala213Version))
  .jsPlatform(
    scalaVersions = List(dottyVersion, scala212Version, scala213Version),
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val cli = projectMatrix
  .in(file("cli"))
  .settings(
    name := "semver4s-cli",
    version := "1.0.2",
    libraryDependencies ++= List(
      "com.monovore" %%% "decline"        % "2.0.0",
      "com.monovore" %%% "decline-effect" % "2.0.0"
    )
  )
  .dependsOn(lib)
  .jvmPlatform(scalaVersions = List(scala212Version, scala213Version))
  .jsPlatform(
    scalaVersions = List(scala212Version, scala213Version),
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val npmfacade = projectMatrix
  .in(file("npmfacade"))
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    testFrameworks += new TestFramework("munit.Framework"),
    name := "npmFacade",
    version := "0.0.1",
    Test / npmDependencies += "semver" -> "7.3.4",
    publish / skip := true,
    libraryDependencies ++= List(
      "org.scalameta"     %%% "munit"            % "0.7.23" % "test",
      "org.scalameta"     %%% "munit-scalacheck" % "0.7.23" % "test",
      "io.github.cquiroz" %%% "scala-java-time"  % "2.2.0"  % "test"
    )
  )
  .jsPlatform(
    scalaVersions = List(dottyVersion, scala213Version),
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )
  .dependsOn(lib % "compile->compile;test->test")
