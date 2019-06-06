import java.nio.file.Files

import scala.collection.JavaConverters._
import sbt.Keys._
import sbt._

object BenchmarkResultsPlugin extends AutoPlugin {

  override def requires = plugins.JvmPlugin

  object autoImport {

    def benchmarkResultsCommand(baseDir: File): Command = {
      val resultDir = (baseDir / "results").toPath

      Command.command("benchmarkResults") { state ⇒
        Analysis(Files.list(resultDir).iterator().asScala.map(_.toFile).filter(_.name endsWith ".json").toVector)
        state
      }
    }
  }
  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    Keys.commands += benchmarkResultsCommand(baseDirectory.value)
  )
}
