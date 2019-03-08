import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import ReleaseTransformations._

lazy val commonSettings = Seq(
  organization := "io.bullet",
  homepage := Some(new URL("https://github.com/sirthias/borer")),
  description := "CBOR (de)serialization in Scala",
  startYear := Some(2019),
  licenses := Seq("MPLv2" → new URL("https://www.mozilla.org/en-US/MPL/2.0/")),
  unmanagedResources in Compile += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(ScmInfo(url("https://github.com/sirthias/borer"), "scm:git:git@github.com:sirthias/borer.git")),

  scalaVersion := "2.12.8",
  crossScalaVersions := Seq(scalaVersion.value /*, "2.13.0-M5" */),

  scalacOptions ++= commonScalacOptions,
  scalacOptions in (Compile, console) ~= (_ filterNot (o ⇒ o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  scalacOptions in (Test, console) ~= (_ filterNot (o ⇒ o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  scalacOptions in (Compile, doc) += "-no-link-warnings",
  sourcesInBase := false,

  // file headers
  headerLicense := Some(HeaderLicense.MPLv2("2019", "Mathias Doenitz")),
  
  // reformat main and test sources on compile
  scalafmtOnCompile := true,

  testFrameworks += new TestFramework("utest.runner.Framework"),
  initialCommands in console := """import io.bullet.borer.core._""",
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:_,-missing-interpolator",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
  "-Xsource:2.13", // new warning: deprecate assignments in argument position
  "-Ycache-macro-class-loader:last-modified",
  "-Ybackend-parallelism", "8")

lazy val crossSettings = Seq(
  sourceDirectories in (Compile, scalafmt) := (unmanagedSourceDirectories in Compile).value,
  sourceDirectories in (Test, scalafmt) := (unmanagedSourceDirectories in Test).value
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  scalaJSStage in Global := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") :+ "-P:scalajs:sjsDefinedByDefault" }
)

lazy val publishingSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ ⇒
    false
  },
  publishTo := sonatypePublishTo.value,
  developers := List(
    Developer("sirthias", "Mathias Doenitz", "devnull@bullet.io", url("https://github.com/sirthias"))
  )
)

lazy val releaseSettings = {
  val runCompile = ReleaseStep(action = { st: State ⇒
    val extracted = Project.extract(st)
    val ref       = extracted.get(thisProjectRef)
    extracted.runAggregated(compile in Compile in ref, st)
  })

  Seq(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runCompile,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      setNextVersion,
      commitNextVersion,
      releaseStepCommand("sonatypeReleaseAll"),
      pushChanges
    )
  )
}

def scalaJsDeps(deps: ModuleID*): Def.Setting[Seq[sbt.ModuleID]] =
  libraryDependencies ++= deps.map(_ cross platformDepsCrossVersion.value)

lazy val macroParadise =
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

/////////////////////// DEPENDENCIES /////////////////////////

val `akka-actor`    = "com.typesafe.akka" %% "akka-actor"    % "2.5.21"
val magnolia        = "com.propensive"    %% "magnolia"      % "0.10.0"
val `scodec-bits`   = "org.scodec"        %% "scodec-bits"   % "1.1.9"
val utest           = "com.lihaoyi"       %% "utest"         % "0.6.6" % "test"
val `scala-reflect` = "org.scala-lang"    %  "scala-reflect"

/////////////////////// PROJECTS /////////////////////////

lazy val borer = project.in(file("."))
  .aggregate(coreJVM, coreJS)
  .aggregate(akka)
  .aggregate(scodecJVM, scodecJS)
  .aggregate(derivationJVM, derivationJS)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(releaseSettings)
  .settings(
    publishArtifact := false,
    sources in (Sbt, scalafmt) := Nil // don't auto-format this file
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin, BoilerplatePlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-core",
    macroParadise,
    scalaJsDeps(utest),
    libraryDependencies += `scala-reflect` % scalaVersion.value,

    // point sbt-boilerplate to the common "project"
    boilerplateSource in Compile := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate",
    sourceManaged in Compile := baseDirectory.value.getParentFile / "target" / "scala" / "src_managed" / "main"
  )
  .jsSettings(scalajsSettings: _*)

lazy val akka = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(coreJVM % "compile->compile;test->test")
  .dependsOn(derivationJVM % "test->compile")
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-akka",
    libraryDependencies ++= Seq(`akka-actor`, utest)
  )

lazy val scodecJVM = scodec.jvm
  .dependsOn(coreJVM % "compile->compile;test->test")
  .dependsOn(derivationJVM % "test->compile")
lazy val scodecJS  = scodec.js
  .dependsOn(coreJS   % "compile->compile;test->test")
  .dependsOn(derivationJS % "test->compile")
lazy val scodec = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-scodec",
    scalaJsDeps(`scodec-bits`, utest)
  )
  .jsSettings(scalajsSettings: _*)

lazy val derivationJVM = derivation.jvm.dependsOn(coreJVM % "compile->compile;test->test")
lazy val derivationJS  = derivation.js.dependsOn(coreJS   % "compile->compile;test->test")
lazy val derivation = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-derivation",
    scalaJsDeps(magnolia, utest)
  )
  .jsSettings(scalajsSettings: _*)
