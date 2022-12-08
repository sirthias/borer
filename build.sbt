import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt._

def scala213 = "2.13.10"

inThisBuild(
  List(
    organization := "io.bullet",
    homepage     := Some(new URL("https://github.com/sirthias/borer/")),
    description  := "CBOR and JSON (de)serialization in Scala",
    startYear    := Some(2019),
    licenses     := Seq("MPLv2" → new URL("https://www.mozilla.org/en-US/MPL/2.0/")),
    scmInfo := Some(ScmInfo(url("https://github.com/sirthias/borer/"), "scm:git:git@github.com:sirthias/borer.git")),
    versionScheme := Some("early-semver"),
    developers :=
      List(
        "sirthias" -> "Mathias Doenitz",
      ).map { case (username, fullName) =>
        Developer(username, fullName, s"@$username", url(s"https://github.com/$username"))
      }
  )
)

lazy val commonSettings = Seq(
  scalaVersion := scala213,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-language:_",
    "-release:9",
    "-Xlint:_,-missing-interpolator",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ybackend-parallelism", "8",
    "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
    "-Ycache-macro-class-loader:last-modified",
    "-Vimplicits",
    "-Vtype-diffs",
  ),
  Compile / console / scalacOptions ~= (_ filterNot (o => o.contains("warn") || o.contains("Xlint"))),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
  Compile / doc / scalacOptions += "-no-link-warnings",
  sourcesInBase := false,
  Compile / unmanagedResources += baseDirectory.value.getParentFile.getParentFile / "LICENSE",

  scalafmtOnCompile := true, // reformat main and test sources on compile

  // file headers
  headerLicense := Some(HeaderLicense.MPLv2("2019-2022", "Mathias Doenitz")),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  console / initialCommands := """import io.bullet.borer._""",

  // publishing
  publishMavenStyle      := true,
  Test / publishArtifact := false,
  pomIncludeRepository   := (_ ⇒ false),
  publishTo              := sonatypePublishToBundle.value,
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  scalaJSStage in Global := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") }
)

lazy val releaseSettings = {
  import ReleaseTransformations._
  Seq(
    releaseCrossBuild := false,
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

/////////////////////// DEPENDENCIES /////////////////////////

// format: OFF
val `akka-actor`        = Def.setting("com.typesafe.akka"       %%  "akka-actor-typed"        % "2.6.20")
val `akka-stream`       = Def.setting("com.typesafe.akka"       %%  "akka-stream"             % "2.6.20")
val `akka-http`         = Def.setting("com.typesafe.akka"       %%  "akka-http"               % "10.4.0")
val `cats-core`         = Def.setting("org.typelevel"           %%% "cats-core"               % "2.9.0")
val `circe-core`        = Def.setting("io.circe"                %%% "circe-core"              % "0.13.0")
val `circe-parser`      = Def.setting("io.circe"                %%% "circe-parser"            % "0.13.0")
val `circe-derivation`  = Def.setting("io.circe"                %%% "circe-derivation"        % "0.13.0-M5")
val `scodec-bits`       = Def.setting("org.scodec"              %%% "scodec-bits"             % "1.1.34")
val utest               = Def.setting("com.lihaoyi"             %%% "utest"                   % "0.8.1" % Test)
val munit               = Def.setting("org.scalameta"           %%% "munit"                   % "0.7.29" % Test)
val macrolizer          = Def.setting("io.bullet"               %%% "macrolizer"              % "0.6.2" % "compile-internal, test-internal")
val `scala-compiler`    = Def.setting("org.scala-lang"          %   "scala-compiler" % scalaVersion.value % "provided")
val `scala-reflect`     = Def.setting("org.scala-lang"          %   "scala-reflect" % scalaVersion.value % "provided")
// format: ON

/////////////////////// PROJECTS /////////////////////////

lazy val borer = (project in file("."))
  .aggregate(`core-jvm`, `core-js`)
  .aggregate(`compat-akka`)
  .aggregate(`compat-cats-jvm`, `compat-cats-js`)
  .aggregate(`compat-circe-jvm`, `compat-circe-js`)
  .aggregate(`compat-scodec-jvm`, `compat-scodec-js`)
  .aggregate(`derivation-jvm`, `derivation-js`)
  .aggregate(deriver)
  .aggregate(benchmarks)
  .aggregate(site)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    publish / skip := true,
    onLoadMessage  := welcomeMessage.value
  )

lazy val `core-jvm` = core.jvm.enablePlugins(SpecializeJsonParserPlugin)
lazy val `core-js`  = core.js
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin, BoilerplatePlugin)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-core",
    libraryDependencies ++= Seq(utest.value),

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
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-cats",
    libraryDependencies ++= Seq(`cats-core`.value, utest.value)
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
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-circe",
    libraryDependencies ++= Seq(
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
  .settings(commonSettings)
  .settings(releaseSettings)
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
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-derivation",
    scalacOptions += "-Wnonunit-statement",
    libraryDependencies ++= Seq(`scala-compiler`.value, `scala-reflect`.value, macrolizer.value),
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
  .dependsOn(`core-jvm`, `derivation-jvm`)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"        % "2.19.0",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"      % "2.19.0" % Provided,
      "com.fasterxml.jackson.module"          %% "jackson-module-scala"       % "2.14.1",
      "com.fasterxml.jackson.module"           % "jackson-module-afterburner" % "2.14.1",
      "com.lihaoyi"                           %% "upickle"                    % "1.6.0",
      "io.spray"                              %% "spray-json"                 % "1.3.6",
      `circe-core`.value,
      `circe-parser`.value,
      `circe-derivation`.value,
    )
  )

lazy val site = project
  .dependsOn(`core-jvm`, `derivation-jvm`, `compat-akka`, `compat-cats-jvm`, `compat-circe-jvm`, `compat-scodec-jvm`)
  .enablePlugins(
    ParadoxPlugin,
    ParadoxMaterialThemePlugin,
    ParadoxSitePlugin,
    GhpagesPlugin
  )
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(`akka-actor`.value, `akka-stream`.value, `akka-http`.value, utest.value),
    com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo := scmInfo.value.get.connection.drop("scm:git:".length),
    ghpagesNoJekyll                               := true,
    ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Compile),
    Compile / paradoxMaterialTheme := {
      ParadoxMaterialTheme()
        .withFavicon("assets/images/favicon.ico")
        .withColor("indigo", "orange")
        .withLogo("assets/images/borer-logo-white.svg")
        .withCustomStylesheet("assets/stylesheets/borer.css")
        .withCopyright("Copyright (C) 2019-2022 Mathias Doenitz")
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
    Compile / paradox / version := "1.8.0",
    paradoxProperties ++= Map(
      "image.base_url" -> ".../assets/images",
      "github.base_url" -> {
        val v = version.value
        s"https://github.com/sirthias/borer/tree/${if (v.endsWith("SNAPSHOT")) "master" else "v" + v}"
      },
      "extref.rfc.base_url" -> "http://tools.ietf.org/html/rfc%s",
      "snip.test.base_dir"  -> s"${(Test / sourceDirectory).value}/scala/io/bullet/borer/site",
      "snip.core.base_dir"  -> s"${baseDirectory.value}/../core/src/main/scala/io/bullet/borer",
    )
  )

// welcome message in the style of zio.dev
def welcomeMessage = Def.setting {
  import scala.Console

  def red(text: String): String  = s"${Console.RED}$text${Console.RESET}"
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
      |${item("project `coreJS`")} - Descend into the JS core module
      |${item("test")} - Run all tests
      |${item("project benchmarks;benchmarkResults;project /")} - Show results of latest benchmark runs
      """.stripMargin
}
