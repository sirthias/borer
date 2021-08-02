import sbt._

def scala3   = "3.0.0"
def scala213 = "2.13.6"
def scala212 = "2.12.14"

lazy val allScalaVersions = Seq(scala212, scala213, scala3)
lazy val scala2Only       = Seq(scala212, scala213)

inThisBuild(
  List(
    organization := "io.bullet",
    homepage     := Some(new URL("https://github.com/sirthias/borer/")),
    description  := "CBOR and JSON (de)serialization in Scala",
    startYear    := Some(2019),
    licenses     := Seq("MPLv2" → new URL("https://www.mozilla.org/en-US/MPL/2.0/")),
    scmInfo := Some(ScmInfo(url("https://github.com/sirthias/borer/"), "scm:git:git@github.com:sirthias/borer.git")),
    developers :=
      List(
        "sirthias" -> "Mathias Doenitz",
      ).map { case (username, fullName) =>
        Developer(username, fullName, s"@$username", url(s"https://github.com/$username"))
      }
  )
)

lazy val commonSettings = Seq(
  scalacOptions ++= {
    Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
    ) ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, minor)) =>
          Seq(
            "-language:_",
            "-target:jvm-1.8",
            "-Xlint:_,-missing-interpolator",
            "-Ywarn-dead-code",
            "-Ywarn-numeric-widen",
            "-Ybackend-parallelism",
            "8",
            "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
            "-Ycache-macro-class-loader:last-modified",
          ) ++ {
            minor match {
              case 12 =>
                Seq(
                  "-Yno-adapted-args",
                  "-Ywarn-inaccessible",
                  "-Ywarn-infer-any",
                  "-Ywarn-nullary-override",
                  "-Ywarn-nullary-unit",
                  "-Xfuture",
                  "-Xsource:2.13",
                )
              case 13 =>
                Seq(
                  "-Xfatal-warnings",
                  "-Vimplicits",
                  "-Vtype-diffs",
                )
            }
          }
        case Some((3, _)) =>
          Seq(
            "-source:3.0-migration",
            "-Xtarget:8",
            "-Xfatal-warnings",
            "-language:implicitConversions",
          )
        case x => sys.error(s"unsupported scala version: $x")
      }
    }
  },
  Compile / console / scalacOptions ~= (_ filterNot (o => o.contains("warn") || o.contains("Xlint"))),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
  Compile / doc / scalacOptions += "-no-link-warnings",
  sourcesInBase := false,
  Compile / unmanagedResources += baseDirectory.value.getParentFile.getParentFile / "LICENSE",

  // file headers
  headerLicense := Some(HeaderLicense.MPLv2("2019-2021", "Mathias Doenitz")),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  console / initialCommands := """import io.bullet.borer._""",

  // publishing
  publishMavenStyle      := true,
  Test / publishArtifact := false,
  pomIncludeRepository   := (_ ⇒ false),
  publishTo              := sonatypePublishToBundle.value,
)

lazy val scalafmtSettings = Seq(
  scalafmtOnCompile := true, // reformat main and test sources on compile
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  scalaJSStage in Global := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") }
)

lazy val releaseSettings = {
  import ReleaseTransformations._
  Seq(
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

val `akka-actor`        = Def.setting("com.typesafe.akka" %% "akka-actor-typed" % "2.6.15")
val `akka-stream`       = Def.setting("com.typesafe.akka" %% "akka-stream" % "2.6.15")
val `akka-http`         = Def.setting("com.typesafe.akka" %% "akka-http" % "10.2.4")
val `collection-compat` = Def.setting("org.scala-lang.modules" %%% "scala-collection-compat" % "2.4.4")
val `cats-core`         = Def.setting("org.typelevel" %%% "cats-core" % "2.6.1")
val `circe-core`        = Def.setting("io.circe" %%% "circe-core" % "0.14.1")
val `circe-parser`      = Def.setting("io.circe" %%% "circe-parser" % "0.14.1")
val `circe-derivation`  = Def.setting("io.circe" %%% "circe-derivation" % "0.13.0-M5")
val `scodec-bits`       = Def.setting("org.scodec" %%% "scodec-bits" % "1.1.27")
val utest               = Def.setting("com.lihaoyi" %%% "utest" % "0.7.10" % "test")
val `scala-compiler`    = Def.setting("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
val `scala-reflect`     = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")

/////////////////////// PROJECTS /////////////////////////

lazy val borer = (project in file("."))
  .aggregate(
    Seq(
      core,
      `compat-akka`,
      `compat-cats`,
      `compat-circe`,
      `compat-scodec`,
      `derivation`,
      deriver,
    ).flatMap(_.projectRefs): _*
  )
  .aggregate(benchmarks)
  .aggregate(site)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    publish / skip := true,
    onLoadMessage  := welcomeMessage.value
  )

lazy val core = (projectMatrix in file("core"))
  .enablePlugins(AutomateHeaderPlugin, BoilerplatePlugin)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-core",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
        case _             => Nil
      }
    },
    libraryDependencies ++= Seq(`collection-compat`.value, utest.value),
  )
  .customRow(
    axisValues = Seq(VirtualAxis.jvm),
    scalaVersions = Seq(scala213),
    process = { _.enablePlugins(SpecializeJsonParserPlugin) }
  )
  .customRow(
    axisValues = Seq(VirtualAxis.jvm),
    scalaVersions = Seq(scala212, scala3),
    process = {
      _.enablePlugins(SpecializeJsonParserPlugin)
        .disablePlugins(ScalafmtPlugin)
    }
  )
  .jsPlatform(allScalaVersions, scalajsSettings)

lazy val `compat-akka` = (projectMatrix in file("compat-akka"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(derivation % "test->compile")
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-akka",
    libraryDependencies ++= Seq(
      `akka-actor`.value  % "provided",
      `akka-stream`.value % "provided",
      `akka-http`.value   % "provided",
      utest.value)
  )
  .jvmPlatform(scalaVersions = scala2Only)

lazy val `compat-cats` = (projectMatrix in file("compat-cats"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(derivation % "test->compile")
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-cats",
    libraryDependencies ++= Seq(
      `collection-compat`.value,
      `cats-core`.value,
      utest.value
    )
  )
  .jvmPlatform(scala2Only)
  .jsPlatform(scala2Only, scalajsSettings)

lazy val `compat-circe` = (projectMatrix in file("compat-circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(derivation % "test->compile")
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-circe",
    libraryDependencies ++= Seq(
      `collection-compat`.value,
      `circe-core`.value,
      `circe-parser`.value     % "test",
      `circe-derivation`.value % "test",
      utest.value
    )
  )
  .jvmPlatform(scala2Only)
  .jsPlatform(scala2Only, scalajsSettings)

lazy val `compat-scodec` = (projectMatrix in file("compat-scodec"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(derivation % "test->compile")
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-compat-scodec",
    libraryDependencies ++= Seq(
      `scodec-bits`.value % "provided",
      utest.value
    )
  )
  .jvmPlatform(scala2Only)
  .jsPlatform(scala2Only, scalajsSettings)

lazy val derivation = (projectMatrix in file("derivation"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(
    deriver
  ) // it would be great to be able to eliminate this dependency for Scala 3, but: https://github.com/sbt/sbt-projectmatrix/issues/55
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-derivation",
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.scalaABIVersion(scala3))) Nil
      else Seq(`scala-compiler`.value, `scala-reflect`.value, utest.value)
    }
  )
  .jvmPlatform(scala2Only)
  .jsPlatform(scala2Only, scalajsSettings)

lazy val deriver = (projectMatrix in file("deriver"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(
    moduleName := "borer-deriver",
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.scalaABIVersion(scala3))) Nil
      else Seq(`scala-compiler`.value, `scala-reflect`.value, utest.value)
    }
  )
  .jvmPlatform(scala2Only)
  .jsPlatform(scala2Only, scalajsSettings)

def s213(matrix: sbt.internal.ProjectMatrix): Project = matrix.finder(VirtualAxis.jvm)(scala213)

lazy val benchmarks = project
  .enablePlugins(AutomateHeaderPlugin, JmhPlugin, BenchmarkResultsPlugin)
  .disablePlugins(MimaPlugin)
  .dependsOn(
    s213(core),
    s213(derivation),
  )
  .settings(commonSettings)
  .settings(
    scalaVersion   := scala213,
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"        % "2.9.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"      % "2.9.1" % Provided,
      "com.fasterxml.jackson.module"          %% "jackson-module-scala"       % "2.12.3",
      "com.fasterxml.jackson.module"           % "jackson-module-afterburner" % "2.12.3",
      "com.lihaoyi"                           %% "upickle"                    % "1.4.0",
      "io.spray"                              %% "spray-json"                 % "1.3.6",
      `circe-core`.value,
      `circe-parser`.value,
      `circe-derivation`.value,
    )
  )

lazy val site = project
  .in(file("site"))
  .dependsOn(
    s213(core),
    s213(derivation),
    s213(`compat-akka`),
    s213(`compat-cats`),
    s213(`compat-circe`),
    s213(`compat-scodec`),
  )
  .enablePlugins(
    ParadoxPlugin,
    ParadoxMaterialThemePlugin,
    ParadoxSitePlugin,
    GhpagesPlugin
  )
  .settings(commonSettings)
  .settings(
    scalaVersion   := scala213,
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
        .withCopyright("Copyright (C) 2019-2021 Mathias Doenitz")
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
    Compile / paradox / version := "1.7.2",
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
