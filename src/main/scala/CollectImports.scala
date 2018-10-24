import java.io.File

object CollectImports {

  type Import = String
  type Stats = Map[Import, Int]

  import scala.io.StdIn.readLine
  import scala.io.Source

  def entry = {
    var i = 0
    val startDir = readLine()
    val stats =
      scalaFiles(getFileTree(new File(startDir))).foldLeft(Map.empty[Import, Int]) {
        case (stats2, file) =>
          i += 1
          if (i%1000 == 0) println(s"Processing ${file.getAbsolutePath} #$i")
          countImports(stats2, Source.fromFile(file)(scala.io.Codec.ISO8859).getLines().toSeq)
      }
    for ((k, v) <- stats.toSeq.sortBy(_._2).reverse) {
      println(s"$k $v")
    }
  }

  def getFileTree(f: File): Stream[File] = {
    f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
    else Stream.empty)
  }

  def scalaFiles(files: Stream[File]): Stream[File] = files filter { f =>
    f.isFile && f.getName.endsWith(".scala")
  }

  def countImports(stats: Stats, lines: Seq[String]): Stats = {
    def touch(stats: Stats, imp: Import): Stats = {
      stats + (imp -> (stats.getOrElse(imp, 0) + 1))
    }

    def explode(line: String): Seq[String] = {
      val mult = """(.*)\{(.*)\}""".r
      line match {
        case mult(prefix, importCol) =>
          importCol.split(',').map { imp =>
            val rename = "(.*)=>.*".r
            val imp2 = imp match {
              case rename(origName) => origName
              case _ => imp
            }
            prefix + imp2.trim
          }
        case _ =>
          // default: couldn't decompose
          Seq(line)
      }
    }

    lines.foldLeft(stats) {
      case (acc, line) =>
        if (isJavaImport(line)) explode(line).foldLeft(acc) { case (acc2, imp) => touch(acc2, imp) }
        else acc
    }
  }

  def isJavaImport(str: String): Boolean = {
    str.startsWith("import java")
  }
}
