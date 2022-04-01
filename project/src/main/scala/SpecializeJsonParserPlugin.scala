import sbt.Keys._
import sbt._

import scala.annotation.tailrec

object SpecializeJsonParserPlugin extends AutoPlugin {

  override def requires = plugins.JvmPlugin

  object autoImport {
    val specializeJsonParser = taskKey[Seq[File]](
      "Generates sources for specializing JSON parsing from byte arrays when sun.misc.Unsafe is available")
  }
  import autoImport._

  override def projectSettings: Seq[Setting[_]] = inConfig(Compile) {
    Seq(
      specializeJsonParser := generateSpecializedJsonParser(
        streams = streams.value,
        sourceDir = (specializeJsonParser / sourceDirectory).value,
        targetDir = (specializeJsonParser / sourceManaged).value.getParentFile / "scala" / "src_managed"),
      sourceGenerators += specializeJsonParser
    )
  }

  val rewrites = Seq(
    FileRewrite(
      _ / "scala" / "io" / "bullet" / "borer" / "input" / "FromByteArrayInput.scala",
      _ / "scala" / "io" / "bullet" / "borer" / "input" / "DirectFromByteArrayInput.scala",
      RewriteRule.DeleteFirst("import io.bullet.borer.internal.ByteArrayAccess"),
      RewriteRule.ReplaceFirst("import io.bullet.borer.{ByteAccess, Input}", "import io.bullet.borer.Input"),
      RewriteRule.DeleteSection("trait FromByteArrayInput", "final private class FromByteArray"),
      RewriteRule.DeleteLast("}"),
      RewriteRule.ReplaceFirst(
        "private class FromByteArray(byteArray: Array[Byte]",
        "private[borer] class DirectFromByteArrayInput(byteArray: Array[Byte], baa: io.bullet.borer.internal.Unsafe.LittleEndianByteArrayAccess"),
      RewriteRule.ReplaceAll("ByteArrayAccess.instance", "baa")
    ),
    FileRewrite(
      _ / "scala" / "io" / "bullet" / "borer" / "json" / "JsonParser.scala",
      _ / "scala" / "io" / "bullet" / "borer" / "json" / "DirectJsonParser.scala",
      RewriteRule.ReplaceFirst("JsonParser[Bytes]", "DirectJsonParser"),
      RewriteRule.ReplaceFirst("Input[Bytes]", "io.bullet.borer.input.DirectFromByteArrayInput"),
      RewriteRule.ReplaceFirst("extends Parser[Bytes]", "extends Parser[Array[Byte]]"),
      RewriteRule.DeleteFirst("(\n    implicit byteAccess: ByteAccess[Bytes])"),
      RewriteRule.ReplaceFirst("padBytes(rest: Bytes", "padBytes(rest: Array[Byte]"),
      RewriteRule.ReplaceFirst("byteAccess.toByteArray(bytes)", "bytes"),
      RewriteRule.Truncate("private[borer] object JsonParser")
    )
  )

  final case class FileRewrite(source: File => File, target: File => File, rules: RewriteRule*)

  sealed abstract class RewriteRule {
    def apply(string: String): Either[String, String]
  }

  object RewriteRule {
    final case class ReplaceFirst(searchString: String, replacement: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        val ix = string.indexOf(searchString)
        if (ix >= 0) Right(string.substring(0, ix) + replacement + string.substring(ix + searchString.length))
        else Left(s"[ReplaceFirst] string `$searchString` not found")
      }
    }
    final case class ReplaceAll(searchString: String, replacement: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        @tailrec def rec(string: String, ix: Int): Either[String, String] = {
          val i = string.indexOf(searchString, ix)
          if (i >= 0) {
            val next = string.substring(0, i) + replacement + string.substring(i + searchString.length)
            rec(next, i + replacement.length)
          } else Right(string)
        }
        rec(string, 0)
      }
    }
    final case class DeleteFirst(searchString: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        val ix = string.indexOf(searchString)
        if (ix >= 0) Right(string.substring(0, ix) + string.substring(ix + searchString.length))
        else Left(s"[DeleteFirst] string `$searchString` not found")
      }
    }
    final case class DeleteLast(searchString: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        val ix = string.lastIndexOf(searchString)
        if (ix >= 0) Right(string.substring(0, ix) + string.substring(ix + searchString.length))
        else Left(s"[DeleteLast] string `$searchString` not found")
      }
    }
    final case class DeleteSection(deletionStart: String, deletionEnd: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        val start = string.indexOf(deletionStart)
        if (start >= 0) {
          val end = string.indexOf(deletionEnd)
          if (end >= 0) Right(string.substring(0, start) + string.substring(end))
          else Left(s"[DeleteSection] deletionEnd `$deletionEnd` not found")
        } else Left(s"[DeleteSection] deletionStart `$deletionStart` not found")
      }
    }

    final case class Truncate(truncationHead: String) extends RewriteRule {
      def apply(string: String): Either[String, String] = {
        val ix = string.indexOf(truncationHead)
        if (ix >= 0) Right(string.substring(0, ix))
        else Left(s"[Truncate] truncationHead `$truncationHead` not found")
      }
    }
  }

  def generateSpecializedJsonParser(streams: TaskStreams, sourceDir: File, targetDir: File): Seq[File] = {
    rewrites.map { case FileRewrite(source, target, rules @ _*) =>
      val sourceFile = source(sourceDir)
      if (!sourceFile.exists()) sys.error(s"Cannot rewrite `$sourceFile`: file not found!")
      val targetFile = target(targetDir)
      if (!targetFile.exists()) {
        val dropLen = sourceDir.toString.zip(targetDir.toString).takeWhile(t => t._1 == t._2).size
        streams.log.info(
          s"Rewriting ../${sourceFile.toString.drop(dropLen)} to ../${targetFile.toString.drop(dropLen)}")
        val fileContent = IO.read(sourceFile)
        val rewritingResult = rules.foldLeft(Right(fileContent): Either[String, String]) {
          case (error @ Left(_), _)  => error
          case (Right(string), rule) => rule(string)
        }
        val rewrittenContent = rewritingResult.fold(err => sys.error(s"Error rewriting `$sourceFile`: $err"), identity)
        IO.write(targetFile, rewrittenContent)
      }
      targetFile
    }
  }
}
