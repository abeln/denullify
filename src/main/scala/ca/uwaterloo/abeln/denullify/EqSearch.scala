package ca.uwaterloo.abeln.denullify

import scala.io.StdIn.readLine

object EqSearch {

  case class EqStats(valEq: Int, valNe: Int, refEq: Int, refNe: Int)
  val zero = EqStats(0, 0, 0, 0)

  def entry(): Unit = {
    val dirName = readLine()

    val res = DirOps.foldLines(dirName, zero) {
      case (stats, filename, line) =>
        val toks = line.split("\\W+").toList
        toks match {
          case _ :: rest =>
            toks.zip(rest).foldLeft(stats) {
              case (acc@EqStats(valEq, valNe, refEq, refNe), (tok, nextTok)) =>
                (tok, nextTok) match {
                  case ("==", "null") | ("null", "==") =>
                    println(s"file: $filename\nline: $line")
                    EqStats(valEq + 1, valNe, refEq, refNe)
                  case ("!=", "null") | ("null", "!=") =>
                    println(s"file: $filename\nline: $line")
                    EqStats(valEq, valNe + 1, refEq, refNe)
                  case ("eq", "null") | ("null", "eq") =>
                    println(s"file: $filename\nline: $line")
                    EqStats(valEq, valNe, refEq + 1, refNe)
                  case ("ne", "null") | ("null", "ne") =>
                    println(s"file: $filename\nline: $line")
                    EqStats(valEq, valNe, refEq, refNe + 1)
                  case _ => acc
                }
            }
          case Nil => stats
        }
    }

    println(s"stats = $res")
  }
}
