import sbt._
import _root_.io.bullet.borer.Json

object Analysis {
  import JmhJsonModel._

  final case class Score(library: String, method: String, param: String, score: Double)

  def apply(jsonFiles: Vector[File]): Unit = {

    val fileModels =
      jsonFiles
        .sortBy(_.name)
        .flatMap(Json.decode(_).to[List[RootInterface]].value)
        .filter(_.benchmark contains "ModelBenchmark")

    val results =
      fileModels.map { model =>
        val libary = {
          val s = model.benchmark.drop("io.bullet.borer.benchmarks.".length)
          s.substring(0, s.indexOf("ModelBenchmark"))
        }
        val method = model.benchmark.substring(model.benchmark.lastIndexOf('.') + 1)
        val param  = model.params.get.fileName
        val score  = model.primaryMetric.score
        Score(libary, method, param, score)
      }

    def showResults(method: String) {
      def perf(library: String): Map[String, Double] =
        results.filter(x => x.library == library && x.method == method).map(x => x.param -> x.score).toMap

      val borerPerf = perf("Borer").toList

      def showLibraryResult(library: String): Unit = {
        val libPerf = perf(library)
        val factors = borerPerf.map { case (param, borerScore) => param -> (borerScore / libPerf(param)) }
        val avg     = factors.map(_._2).sum / factors.size
        val mean = {
          val sorted = factors.sortBy(_._2)
          val mid    = factors.size / 2
          if ((factors.size & 1) == 1) sorted(mid)._2
          else (sorted(mid)._2 + sorted(mid + 1)._2) / 2
        }
        val (minParam, minFactor) = factors.minBy(_._2)
        val (maxParam, maxFactor) = factors.maxBy(_._2)
        println(
          f"""$library:
             |  min: $minFactor%.2f ($minParam)
             |  avg: $avg%.2f
             |  max: $maxFactor%.2f ($maxParam)
             |  mean: $mean%.2f
          """.stripMargin
        )
      }

      println()
      println("Borer performance factors (i.e. by what factor does Borer outperform the ")
      println(s"respective lib over a set of ${borerPerf.size} diverse, real-world JSON data files):")
      println()
      showLibraryResult("Circe")
      showLibraryResult("Jackson")
      showLibraryResult("JsoniterScala")
      showLibraryResult("Spray")
      showLibraryResult("UPickle")
    }

    println()
    println("ENCODING")
    println("========")
    showResults("encodeModel")

    println()
    println("DECODING")
    println("========")
    showResults("decodeModel")
  }
}
