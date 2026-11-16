import java.nio.file.Files

import scala.jdk.CollectionConverters.*
import sbt.Keys.*
import sbt.*

object BenchmarkResultsPlugin extends AutoPlugin {

  override def requires = plugins.JvmPlugin

  object autoImport {

    def benchmarkResultsCommand(baseDir: File): Command = {
      val resultDir = (baseDir / "results").toPath

      Command.command("benchmarkResults") { state =>
        Analysis(Files.list(resultDir).iterator().asScala.map(_.toFile).filter(_.name.endsWith(".json")).toVector)
        state
      }
    }
  }
  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    Keys.commands += benchmarkResultsCommand(baseDirectory.value)
  )
}
