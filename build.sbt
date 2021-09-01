import xerial.sbt.Sonatype._

val scala3Version   = "3.0.2"
val scala213Version = "2.13.5"
val scala212Version = "2.12.13"

val allScalaVersions = List(scala3Version, scala212Version, scala213Version)

Global / semanticdbEnabled    := true
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / testFrameworks += new TestFramework("munit.Framework")
ThisBuild / scalaVersion  := scala213Version
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization  := "com.heroestools"
ThisBuild / licenses      := Seq("LGPLv3" -> url("https://www.gnu.org/licenses/lgpl-3.0.en.html"))
ThisBuild / developers := List(
  Developer(
    id = "Martijn",
    name = "Martijn Hoekstra",
    email = "martijnhoekstra@gmail.com",
    url = url("https://www.github.com")
  )
)

//to start server with debugging in breakpoint:
//ThisBuild / Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--inspect-brk")))

def onCI = sys.env.contains("CI")

//attempt not to starve memory, but limitAll(1) is prone to deadlock
ThisBuild / concurrentRestrictions ++= {
  if (onCI) List(Tags.limitAll(2)) else Nil
}

val batchModeOnCI =
  if (onCI) List(scalaJSLinkerConfig ~= {
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

//core datatypes, no parsing, no cats
lazy val core = projectMatrix
  .in(file("core"))
  .settings(
    name    := "semver4s-core",
    version := "1.0.0",
    scalacOptions --= List("-Xfatal-warnings").filter(_ => scalaVersion.value.startsWith("3")),
    publishTo := sonatypePublishToBundle.value,
    sonatypeProjectHosting := Some(
      GitHubHosting("martijnhoekstra", "semver4s", "martijnhoekstra@gmail.com")
    )
  )
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

//one-stop all-in-one lib, depends on cats and parsing
lazy val lib = projectMatrix
  .in(file("semver4s"))
  .settings(
    name    := "semver4s",
    version := "2.0.0",
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
  .dependsOn(catsParse)
  .dependsOn(catsParseReporter)
  .dependsOn(core)
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val cli = projectMatrix
  .in(file("cli"))
  .settings(
    name    := "semver4s-cli",
    version := "1.0.2",
    libraryDependencies ++= List(
      "com.monovore" %%% "decline"        % "2.1.0",
      "com.monovore" %%% "decline-effect" % "2.1.0"
    )
  )
  .dependsOn(lib)
  .dependsOn(catsParseReporter)
  .jvmPlatform(scalaVersions = List(scala3Version))
  .jsPlatform(
    scalaVersions = List(scala3Version),
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val catsParse = projectMatrix
  .in(file("catsParse"))
  .settings(
    name    := "semver4s-cats-parse",
    version := "1.0.0",
    libraryDependencies ++= List("org.typelevel" %%% "cats-parse" % "0.3.4")
  )
  .dependsOn(core)
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val npmEquivalence = projectMatrix
  .in(file("npmEquivalence"))
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    //testFrameworks += new TestFramework("munit.Framework"),
    name                               := "npmFacade",
    Test / npmDependencies += "semver" -> "7.3.5",
    publish / skip                     := true,
    libraryDependencies ++= List(
      "org.scalameta"     %%% "munit"            % "0.7.28" % "test",
      "org.scalameta"     %%% "munit-scalacheck" % "0.7.28" % "test",
      "io.github.cquiroz" %%% "scala-java-time"  % "2.3.0"  % "test"
    )
  )
  .dependsOn(tests % "compile->compile;test->test")
  .jsPlatform(
    scalaVersions = List(scala3Version),
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )

lazy val tests = projectMatrix
  .in(file("tests"))
  .settings(
    //testFrameworks += new TestFramework("munit.Framework"),
    name := "tests",
    scalacOptions -= "-Xfatal-warnings",
    publish / skip := true,
    libraryDependencies ++= List(
      "org.scalameta"     %%% "munit"            % "0.7.28" % "test",
      "org.scalameta"     %%% "munit-scalacheck" % "0.7.28" % "test",
      "io.github.cquiroz" %%% "scala-java-time"  % "2.3.0"  % "test"
    )
  )
  .dependsOn(lib)
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings = (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) ::
      batchModeOnCI
  )

lazy val catsParseReporter = projectMatrix
  .in(file("reporter"))
  .settings(
    //testFrameworks += new TestFramework("munit.Framework"),
    version := "0.1.0-SNAPSHOT",
    name    := "catsParseReporter",
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-parse"       % "0.3.4",
      "org.scalameta" %%% "munit"            % "0.7.28" % "test",
      "org.scalameta" %%% "munit-scalacheck" % "0.7.28" % "test"
    )
  )
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings =
      (scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }) :: batchModeOnCI
  )
