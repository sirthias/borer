import java.nio.file.{FileSystems, Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Try
import sbt.Keys.*
import sbt.*
import sbt.internal.util.complete.DefaultParsers.*

object BenchmarkResultsPlugin extends AutoPlugin {

  override def requires = plugins.JvmPlugin

  object autoImport {

    /**
     * `benchmarkResults [glob]`
     *
     * Analyses the JMH JSON result files in `<baseDir>/results` whose names match the given glob pattern
     * (e.g. `2026-11-17_*` or `*_v1.1.0.json`; the `.json` extension may be omitted).
     * Without a pattern only the newest file (i.e. the last one by name, since names are date-prefixed) is used,
     * so that results from different runs aren't mixed by accident.
     */
    def benchmarkResultsCommand(baseDir: File): Command = {
      val resultDir = baseDir / "results"

      def resultFiles(): Vector[File] =
        Try(Files.list(resultDir.toPath).iterator().asScala.map(_.toFile).toVector)
          .getOrElse(Vector.empty)
          .filter(f => f.isFile && f.name.endsWith(".json"))
          .sortBy(_.name)

      val parser: State => sbt.internal.util.complete.Parser[Option[String]] = _ =>
        (Space ~> token(NotSpace.examples(resultFiles().map(_.name).toSet, check = false), "<glob>")).?

      Command(
        "benchmarkResults",
        ("benchmarkResults [<glob>]", "Analyse benchmark results (default: newest result file only)"),
        """benchmarkResults [<glob>]
          |
          |  Analyses the JMH JSON result files in `benchmarks/results` whose names match the given
          |  glob pattern (e.g. `2026-11-17_*`, `*_v1.1.0.json`; the `.json` extension may be omitted).
          |  Without a pattern only the newest file (last by name) is analysed.""".stripMargin
      )(parser) { (state, pattern) =>
        val all      = resultFiles()
        val selected = pattern match {
          case None       => all.lastOption.toVector
          case Some(glob) =>
            val matcher = FileSystems.getDefault.getPathMatcher(s"glob:$glob")
            all.filter { f =>
              matcher.matches(Paths.get(f.name)) || matcher.matches(Paths.get(f.name.stripSuffix(".json")))
            }
        }
        if (selected.isEmpty) {
          val what = pattern.fold(s"No result files found in $resultDir")(p => s"No result files matching `$p`")
          state.log.error(s"$what. Available: ${if (all.isEmpty) "none" else all.map(_.name).mkString(", ")}")
          state.fail
        } else {
          state.log.info(s"Analysing ${selected.size} result file(s): ${selected.map(_.name).mkString(", ")}")
          Analysis(selected)
          state
        }
      }
    }
  }
  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    Keys.commands += benchmarkResultsCommand(baseDirectory.value)
  )
}
