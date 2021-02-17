import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt._

def scala213 = "2.13.3"
def scala212 = "2.12.12"

lazy val commonSettings = Seq(
  organization := "io.bullet",
  homepage := Some(new URL("https://github.com/sirthias/borer/")),
  description := "CBOR and JSON (de)serialization in Scala",
  startYear := Some(2019),
  licenses := Seq("MPLv2" → new URL("https://www.mozilla.org/en-US/MPL/2.0/")),
  unmanagedResources in Compile += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(ScmInfo(url("https://github.com/sirthias/borer/"), "scm:git:git@github.com:sirthias/borer.git")),

  scalaVersion := scala213,
  crossScalaVersions := Seq(scala212, scala213),

  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked",
    "-target:jvm-1.8",
    "-Xlint:_,-missing-interpolator",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ybackend-parallelism", "8",
    "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
    "-Ycache-macro-class-loader:last-modified",
  ),// ++ (if (sys.props("java.version") startsWith "1." /* i.e. Java version < 9 */) Nil else Seq("-release", "8")),
  
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(
        "-Yno-adapted-args",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Xfuture",
        "-Xsource:2.13",
      )
      case Some((2, 13)) => Nil
      case x => sys.error(s"unsupported scala version: $x")
    }
  },

  scalacOptions in (Compile, console) ~= (_ filterNot(o => o.contains("warn") || o.contains("Xlint"))),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalacOptions in (Compile, doc) += "-no-link-warnings",
  sourcesInBase := false,

  // file headers
  headerLicense := Some(HeaderLicense.MPLv2("2019", "Mathias Doenitz")),

  // reformat main and test sources on compile
  scalafmtOnCompile := true,

  testFrameworks += new TestFramework("utest.runner.Framework"),
  initialCommands in console := """import io.bullet.borer._""",

  // publishing
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ ⇒ false),
  publishTo := sonatypePublishToBundle.value,
  mimaFailOnNoPrevious := false,

  // we need this resolver to let Travis CI find new release artifacts before they are available from Maven Central
  resolvers += Resolver.sonatypeRepo("staging"),

  developers := List(
    Developer("sirthias", "Mathias Doenitz", "devnull@bullet.io", url("https://github.com/sirthias/"))
  ),

  // test coverage
  coverageMinimum := 90,
  coverageFailOnMinimum := false,

  commands += Command.command("openCoverageReport") { state =>
    val uri = s"file://${Project.extract(state).get(crossTarget)}/scoverage-report/index.html"
    state.log.info(s"Opening browser at $uri ...")
    java.awt.Desktop.getDesktop.browse(new java.net.URI(uri))
    state
  },

  Global / excludeLintKeys ++= Set(
    scalafmt / sourceDirectories,
    releaseProcess,
    releaseCrossBuild,
    mimaCheckDirection,
    mimaFailOnNoPrevious,
    mimaPreviousArtifacts
  )
)

lazy val mimaSettings = {
  import com.typesafe.tools.mima.core._

  lazy val oldVersionString = scala.sys.process.Process("git describe --abbrev=0").!!.trim.dropWhile(_ == 'v')
  lazy val oldVersion = oldVersionString.split('.').toList
  val newVersion = Def.setting(version.value.split('.').toList)

  Seq(
    mimaCheckDirection := {
      val isPatch = newVersion.value.take(2) == oldVersion.take(2)
      if (isPatch) "both" else "backward"
    },
    mimaPreviousArtifacts := {
      //val isMajorVersionBump = newVersion.value.head != oldVersion.head
      val isPatch = newVersion.value.take(2) == oldVersion.take(2)
      if (isPatch) Set(organization.value %% moduleName.value % oldVersionString)
      else Set.empty // no mima-checking for non-patch releases
    },
    mimaBinaryIssueFilters := Seq( // known binary compatibility issues or internal API to ignore
      ProblemFilters.exclude[ReversedMissingMethodProblem]("*") // we're lenient here: adding methods is fine everywhere
    )
  )
}

lazy val crossSettings = Seq(
  sourceDirectories in (Compile, scalafmt) := (unmanagedSourceDirectories in Compile).value,
  sourceDirectories in (Test, scalafmt) := (unmanagedSourceDirectories in Test).value
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  scalaJSStage in Global := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") }
)

lazy val releaseSettings = {
  import ReleaseTransformations._
  Seq(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeBundleRelease"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
}

lazy val macroParadise =
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
      case _ => Nil
    }
  }

def addCommandsAlias(name: String, cmds: Seq[String]) = addCommandAlias(name, cmds.mkString(";", ";", ""))

addCommandsAlias(
  "testCoverage",
  Seq(
    "clean",
    "coverage",
    "test",
    "coverageReport",
    "openCoverageReport"
  )
)

// used by Travis CI
addCommandsAlias(
  "validate",
  Seq(
    "clean",
    "headerCheck",
    "scalafmtCheck",
    "test:scalafmtCheck",

    // Scala 2.13
    s"++$scala213",
    "test:compile",
    "test",
    "mimaReportBinaryIssues",

    // Scala 2.12
    s"++$scala212",
    "test:compile",
    "test",
    "mimaReportBinaryIssues",

    // establish test coverage (only on JVM projects)
    "coverage",
    "core/test",
    "derivation/test",
    "compat-akka/test",
    "compat-cats/test",
    "compat-circe/test",
    "compat-scodec/test",
    "core/coverageReport",
    "derivation/coverageReport",
    "compat-akka/coverageReport",
    "compat-cats/coverageReport",
    "compat-circe/coverageReport",
    "compat-scodec/coverageReport",
    "coverageOff",
  )
)

/////////////////////// DEPENDENCIES /////////////////////////

val `akka-actor`        = Def.setting("com.typesafe.akka"      %%  "akka-actor-typed"        % "2.6.10")
val `akka-stream`       = Def.setting("com.typesafe.akka"      %%  "akka-stream"             % "2.6.10")
val `akka-http`         = Def.setting("com.typesafe.akka"      %%  "akka-http"               % "10.2.2")
val `collection-compat` = Def.setting("org.scala-lang.modules" %%% "scala-collection-compat" % "2.3.1")
val `cats-core`         = Def.setting("org.typelevel"          %%% "cats-core"               % "2.3.0")
val `circe-core`        = Def.setting("io.circe"               %%% "circe-core"              % "0.13.0")
val `circe-parser`      = Def.setting("io.circe"               %%% "circe-parser"            % "0.13.0")
val `circe-derivation`  = Def.setting("io.circe"               %%% "circe-derivation"        % "0.13.0-M5")
val `scodec-bits`       = Def.setting("org.scodec"             %%% "scodec-bits"             % "1.1.24")
val utest               = Def.setting("com.lihaoyi"            %%% "utest"                   % "0.7.5"  % "test")
val `scala-compiler`    = Def.setting("org.scala-lang"         %  "scala-compiler"           % scalaVersion.value % "provided")
val `scala-reflect`     = Def.setting("org.scala-lang"         %  "scala-reflect"            % scalaVersion.value % "provided")

/////////////////////// PROJECTS /////////////////////////

lazy val borer = project.in(file("."))
  .aggregate(`core-jvm`, `core-js`)
  .aggregate(`compat-akka`)
  .aggregate(`compat-cats-jvm`, `compat-cats-js`)
  .aggregate(`compat-circe-jvm`, `compat-circe-js`)
  .aggregate(`compat-scodec-jvm`, `compat-scodec-js`)
  .aggregate(`derivation-jvm`, `derivation-js`)
  .aggregate(deriver)
  .aggregate(benchmarks)
  .aggregate(site)
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    publish / skip := true,
    onLoadMessage := welcomeMessage.value
  )

lazy val `core-jvm` = core.jvm.enablePlugins(SpecializeJsonParserPlugin)
lazy val `core-js`  = core.js.disablePlugins(MimaPlugin)
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin, BoilerplatePlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-core",
    macroParadise,
    libraryDependencies ++= Seq(`collection-compat`.value, `scala-reflect`.value, utest.value),

    // point sbt-boilerplate to the common "project"
    Compile / boilerplateSource := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate",
    Compile / sourceManaged := baseDirectory.value.getParentFile / "target" / "scala" / "src_managed" / "main"
  )
  .jvmSettings(
    Compile / specializeJsonParser / sourceDirectory := baseDirectory.value.getParentFile / "src" / "main",
    Compile / specializeJsonParser / sourceManaged := baseDirectory.value / "target" / "scala" / "src_managed" / "main",
    Compile / managedSourceDirectories += (Compile / specializeJsonParser / sourceManaged).value
  )
  .jsSettings(scalajsSettings: _*)

lazy val `compat-akka` = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(`core-jvm` % "compile->compile;test->test")
  .dependsOn(`derivation-jvm` % "test->compile")
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-compat-akka",
    libraryDependencies ++= Seq(
      `akka-actor`.value % "provided",
      `akka-stream`.value % "provided",
      `akka-http`.value % "provided",
      utest.value)
  )

lazy val `compat-cats-jvm` = `compat-cats`.jvm
  .dependsOn(`core-jvm` % "compile->compile;test->test")
  .dependsOn(`derivation-jvm` % "test->compile")
lazy val `compat-cats-js`  = `compat-cats`.js
  .dependsOn(`core-js`   % "compile->compile;test->test")
  .dependsOn(`derivation-js` % "test->compile")
lazy val `compat-cats` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-compat-cats",
    libraryDependencies ++= Seq(
      `collection-compat`.value,
      `cats-core`.value,
      utest.value
    )
  )
  .jsSettings(scalajsSettings: _*)

lazy val `compat-circe-jvm` = `compat-circe`.jvm
  .dependsOn(`core-jvm` % "compile->compile;test->test")
  .dependsOn(`derivation-jvm` % "test->compile")
lazy val `compat-circe-js`  = `compat-circe`.js
  .dependsOn(`core-js`   % "compile->compile;test->test")
  .dependsOn(`derivation-js` % "test->compile")
lazy val `compat-circe` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-compat-circe",
    libraryDependencies ++= Seq(
      `collection-compat`.value,
      `circe-core`.value,
      `circe-parser`.value % "test",
      `circe-derivation`.value % "test",
      utest.value
    )
  )
  .jsSettings(scalajsSettings: _*)

lazy val `compat-scodec-jvm` = `compat-scodec`.jvm
  .dependsOn(`core-jvm` % "compile->compile;test->test")
  .dependsOn(`derivation-jvm` % "test->compile")
lazy val `compat-scodec-js`  = `compat-scodec`.js
  .dependsOn(`core-js`   % "compile->compile;test->test")
  .dependsOn(`derivation-js` % "test->compile")
lazy val `compat-scodec` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-compat-scodec",
    libraryDependencies ++= Seq(
      `scodec-bits`.value % "provided",
      utest.value
    )
  )
  .jsSettings(scalajsSettings: _*)

lazy val `derivation-jvm` = derivation.jvm
  .dependsOn(deriver)
  .dependsOn(`core-jvm` % "compile->compile;test->test")
lazy val `derivation-js`  = derivation.js
  .dependsOn(deriver)
  .dependsOn(`core-js` % "compile->compile;test->test")
lazy val derivation = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin)
  .settings(crossSettings)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(mimaSettings)
  .settings(
    moduleName := "borer-derivation",
    libraryDependencies ++= Seq(`scala-compiler`.value, `scala-reflect`.value, utest.value),
  )
  .jsSettings(scalajsSettings: _*)

lazy val deriver = project
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-deriver",
    libraryDependencies ++= Seq(`scala-compiler`.value, `scala-reflect`.value),
  )

lazy val benchmarks = project
  .enablePlugins(AutomateHeaderPlugin, JmhPlugin, BenchmarkResultsPlugin)
  .disablePlugins(MimaPlugin)
  .dependsOn(`core-jvm`, `derivation-jvm`)
  .settings(commonSettings)
  .settings(
    skip in publish := true,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"        % "2.6.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"      % "2.6.1" % Provided,
      "com.fasterxml.jackson.module"          %% "jackson-module-scala"       % "2.12.0",
      "com.fasterxml.jackson.module"          %  "jackson-module-afterburner" % "2.12.0",
      "com.lihaoyi"                           %% "upickle"                    % "1.2.2",
      "io.spray"                              %% "spray-json"                 % "1.3.6",
      `circe-core`.value,
      `circe-parser`.value,
      `circe-derivation`.value,
    )
  )

lazy val site = project
  .in(file("site"))
  .dependsOn(`core-jvm`, `derivation-jvm`, `compat-akka`, `compat-cats-jvm`, `compat-circe-jvm`, `compat-scodec-jvm`)
  .enablePlugins(
    ParadoxPlugin,
    ParadoxMaterialThemePlugin,
    ParadoxSitePlugin,
    GhpagesPlugin
  )
  .disablePlugins(MimaPlugin)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(`akka-actor`.value, `akka-stream`.value, `akka-http`.value, utest.value),

    com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo := scmInfo.value.get.connection.drop("scm:git:".length),
    ghpagesNoJekyll := true,

    ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Compile),
    Compile / paradoxMaterialTheme := {
      ParadoxMaterialTheme()
        .withFavicon("assets/images/favicon.ico")
        .withColor("indigo", "orange")
        .withLogo("assets/images/borer-logo-white.svg")
        .withCustomStylesheet("assets/stylesheets/borer.css")
        .withCopyright("Copyright (C) 2019-2020 Mathias Doenitz")
        .withRepository(scmInfo.value.get.browseUrl.toURI)
        .withSocial(uri("https://github.com/sirthias"), uri("https://twitter.com/sirthias"))
        .withSearch()
    },

    commands += Command.command("openSite") { state =>
      val uri = s"file://${Project.extract(state).get(Compile / paradox / target)}/index.html"
      state.log.info(s"Opening browser at $uri ...")
      java.awt.Desktop.getDesktop.browse(new java.net.URI(uri))
      state
    },

    Compile / paradox / version := "1.6.3",

    paradoxProperties ++= Map(
      "image.base_url" -> ".../assets/images",
      "github.base_url" -> {
        val v = version.value
        s"https://github.com/sirthias/borer/tree/${if (v.endsWith("SNAPSHOT")) "master" else "v" + v}"
      },
      "extref.rfc.base_url" -> "http://tools.ietf.org/html/rfc%s",
      "snip.test.base_dir" -> s"${(Test / sourceDirectory).value}/scala/io/bullet/borer/site",
      "snip.core.base_dir" -> s"${baseDirectory.value}/../core/src/main/scala/io/bullet/borer",
    )
  )

// welcome message in the style of zio.dev
def welcomeMessage = Def.setting {
  import scala.Console

  def red(text: String): String = s"${Console.RED}$text${Console.RESET}"
  def item(text: String): String = s"${Console.GREEN}▶ ${Console.CYAN}$text${Console.RESET}"

  s"""|${red(" _                            ")}
      |${red("| |                           ")}
      |${red("| |__   ___  _ __ ___ _ __    ")}
      |${red("| '_ \\ / _ \\| '__/ _ \\ '__|")}
      |${red("| |_) | (_) | | |  __/ |      ")}
      |${red("|_.__/ \\___/|_|  \\___|_|    " + version.value)}
      |
      |Useful sbt tasks:
      |${item("project core")} - Descend into the JVM core module
      |${item("project `core-js`")} - Descend into the JS core module
      |${item("test")} - Run all tests
      |${item("project benchmarks;benchmarkResults;project /")} - Show results of latest benchmark runs
      """.stripMargin
}