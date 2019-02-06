package ca.uwaterloo.abeln.denullify

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object MigrateTest {

  import scala.io.StdIn.readLine

  def patchTestsFromStdin(): Unit = {
    val fileNames = Iterator.continually(readLine).takeWhile(_ != null).toList.map(_.trim)
    for (f <- fileNames) {
      val patched = patchTest(Source.fromFile(f))
      val file = new File(f)
      val printer = new PrintWriter(file)
      try {
        printer.write(patched.assemble)
        printer.write("\n")
        printer.flush()
      } finally { printer.close() }
    }
  }

  case class FileContents(lines: Seq[String]) {
    def assemble: String = lines.mkString("\n")
  }

  def patchTest(test: Source): FileContents = {
    FileContents(test.getLines().map(patchLine).toSeq)
  }

  def patchLine(line: String): String = {
    type Rule = Regex.Match => (String /* replacement */, String /* type */)
    val rules: Seq[(Regex, Rule)] = Seq(
      ("""null\s*:\s*(\w+|\w+[.*])""".r, m => (s"??? : ${m.group(1)}", m.group(1))), // ascription
      ("""(val|var)\s+(\w+)\s*:\s*(.+)=\s*null""".r, m => (s"${m.group(1)} ${m.group(2)}: ${m.group(3).trim} = ???", m.group(3).trim)), // vals and vars
      ("""(.*def.*\w+(\(.+\))?.*):(.+)=\s*null""".r, m => (s"${m.group(1)}: ${m.group(3).trim} = ???", m.group(3).trim)) // defs
    )
    def unlessNullTpe(rule: Rule)(m: Match): Option[String] = {
      val (repl, tpe) = rule(m)
      tpe match {
        case "Null" => None
        case _ => Some(repl)
      }
    }
    rules.foldLeft(line) { case (l, (re, t)) =>
      re.replaceSomeIn(l, unlessNullTpe(t))
    }
  }
}
