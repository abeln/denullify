import java.io.File

import scala.io.Source

object DirOps {
  def foldLines[T](dirName: String, base: T)(fun: (T, String, String) => T): T = {
      scalaFiles(getFileTree(new File(dirName))).foldLeft(base) {
        case (baseAcc, file) =>
          val lines = Source.fromFile(file)(scala.io.Codec.ISO8859).getLines().toSeq
          lines.foldLeft(baseAcc) {
            case (acc, line) => fun(acc, file.getAbsolutePath, line)
          }
      }
  }

  private def getFileTree(f: File): Stream[File] = {
    f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
    else Stream.empty)
  }

  private def scalaFiles(files: Stream[File]): Stream[File] = files filter { f =>
    f.isFile && f.getName.endsWith(".scala")
  }
}
