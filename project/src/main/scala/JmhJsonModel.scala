import io.bullet.borer.derivation.MapBasedCodecs

object JmhJsonModel {
  import MapBasedCodecs.*
  import io.bullet.borer.Codec

  final case class Params(fileName: String)
  implicit val paramsCodec: Codec[Params] = deriveCodec[Params]

  final case class ScorePercentiles(
      `0.0`: Double,
      `50.0`: Double,
      `90.0`: Double,
      `95.0`: Double,
      `99.0`: Double,
      `99.9`: Double,
      `99.99`: Double,
      `99.999`: Double,
      `99.9999`: Double,
      `100.0`: Double
  )
  implicit val scorePercentilesCodec: Codec[ScorePercentiles] = deriveCodec[ScorePercentiles]

  final case class PrimaryMetric(
      score: Double,
      scoreError: Double,
      scoreConfidence: Seq[Double],
      scorePercentiles: ScorePercentiles,
      scoreUnit: String,
      rawData: Seq[Seq[Double]]
  )
  implicit val primaryMetricCodec: Codec[PrimaryMetric] = deriveCodec[PrimaryMetric]

  case class RootInterface(
      jmhVersion: String,
      benchmark: String,
      mode: String,
      threads: Int,
      forks: Int,
      jvm: String,
      jvmArgs: Seq[String],
      jdkVersion: String,
      vmName: String,
      vmVersion: String,
      warmupIterations: Int,
      warmupTime: String,
      warmupBatchSize: Int,
      measurementIterations: Int,
      measurementTime: String,
      measurementBatchSize: Int,
      primaryMetric: PrimaryMetric,
      params: Option[Params] = None
  )

  implicit val rootInterfaceCodec: Codec[RootInterface] = deriveCodec[RootInterface]
}
