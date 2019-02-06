package ca.uwaterloo.abeln.denullify

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec, _}

import scala.io.Source

class MigrateTestSpec extends FlatSpec {

  import ca.uwaterloo.abeln.denullify.MigrateTest.patchTest

  implicit def stringToSource(s: String): Source = Source.fromString(s)

  "The migration tool" should "handle ascriptions" in {
    patchTest(
      """
        |case class MemoEntry[+T](var r: Either[Nothing,ParseResult[_]])
        |
        |object Test {
        |  def grow[T]: ParseResult[T] = (null: MemoEntry[T]) match {
        |    case MemoEntry(Right(x: ParseResult[_])) => x.asInstanceOf[ParseResult[T]]
        |  }
      """.stripMargin).assemble should equal(
      """
        |case class MemoEntry[+T](var r: Either[Nothing,ParseResult[_]])
        |
        |object Test {
        |  def grow[T]: ParseResult[T] = (??? : MemoEntry[T]) match {
        |    case MemoEntry(Right(x: ParseResult[_])) => x.asInstanceOf[ParseResult[T]]
        |  }
      """.stripMargin)
  }

  it should "handle ascriptions without parens" in {
    patchTest(
      """
        |    map1(((1, null: Coffees), 1))
        |    map1(((null: Coffees, 1), 1))
      """.stripMargin).assemble should equal(
      """
        |    map1(((1, ??? : Coffees), 1))
        |    map1(((??? : Coffees, 1), 1))
      """.stripMargin
    )
  }

  it should "convert vars and vals" in {
    val pt = patchTest(
      """
        |package object foo {
        |
        |  var labels: Array[_ <: String] = null
        |  val labels2: Array[_ <: String] = null
        |
        |}
      """.stripMargin).assemble
    patchTest(
      """
        |package object foo {
        |
        |  var labels: Array[_ <: String] = null
        |  val labels2: Array[_ <: String] = null
        |
        |}
      """.stripMargin).assemble should equal(
      """
        |package object foo {
        |
        |  var labels: Array[_ <: String] = ???
        |  val labels2: Array[_ <: String] = ???
        |
        |}
      """.stripMargin
    )
  }

  it should "not convert ascriptions when the type is Null" in {
    patchTest(
      """
        | map1(((1, null: Null), 1))
      """.stripMargin).assemble should equal(
      """
        | map1(((1, null: Null), 1))
      """.stripMargin
    )
  }

  it should "not alter source that doesn't match any rules" in {
    val code =
      """
        |  def patchTestsFromStdin(): Unit = {
        |    val fileNames = Iterator.continually(readLine).takeWhile(_ != null).toList.map(_.trim)
        |    for (f <- fileNames) {
        |      val patched = patchTest(Source.fromFile(f))
        |      val file = new File(f)
        |      val printer = new PrintWriter(file)
        |      try { printer.write(patched.assemble) } finally { printer.close() }
        |    }
        |  }
      """.stripMargin
    patchTest(code).assemble should equal(code)
  }

  it should "handle defs" in {
    patchTest(
      """
        | implicit def foo(x: String): Foo = null
      """.stripMargin).assemble should equal(
      """
        | implicit def foo(x: String): Foo = ???
      """.stripMargin
    )
  }
}
