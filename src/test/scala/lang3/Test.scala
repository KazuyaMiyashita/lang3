package lang3

import lang3.AST.{Literal, VDouble}
import org.scalactic.source
import org.scalatest.flatspec.AnyFlatSpec

class Test extends AnyFlatSpec {

  private def test(program: String, expected: Literal) = {
    val parseResult = Parser.run(program)
    if (!parseResult.successful) {
      fail(parseResult.toString)
    } else {
      assert(Interpreter.run(parseResult.get) == expected)
    }
  }

  it should "work 1" in {
    val program =
      """{
        |  val a = 1 + 2 * 3
        |  function max(a, b) {
        |    if (a >= b) a else b
        |  }
        |  val b = max(a, 8)
        |  b
        |}
        |""".stripMargin
    test(program, VDouble(8))
  }

  it should "work 2" in {
    val program =
      """{
        |  function acc(v) {
        |    if (v <= 100) {
        |      v + acc(v + 1)
        |  	} else {
        |  	  0
        |  	}
        |  }
        |  val sum = acc(1)
        |  sum
        |}
        |""".stripMargin
    test(program, VDouble(5050))
  }

  it should "work 3" in {
    val program =
      """{
        |  function a() {
        |    42
        |  }
        |  a()
        |}
        |""".stripMargin
    test(program, VDouble(42))
  }

  it should "work 4-1" in {
    val program =
      """{
        |  if({
        |    val a = 42
        |    a
        |  } == 42) true else false
        |}
        |""".stripMargin
    test(program, VDouble(42))
  }

  it should "work 4-2" in {
    val program =
      """{
        |  if({42} == 42) true else false
        |}
        |""".stripMargin
    test(program, VDouble(42))
  }

  it should "work 4-3" in {
    val program =
      """{
        |  if(42 == {42}) true else false
        |}
        |""".stripMargin
    test(program, VDouble(42))
  }

  it should "work 5" in {
    val program =
      """{
        |  function fib(v) {
        |    if (v <= 1) 0
        |    else if (v == 2) 1
        |    else fib(v - 1) + fib(v - 2)
        |  }
        |  fib(10)
        |}
        |""".stripMargin
    test(program, VDouble(34))
  }

  it should "work 6" in {
    val program =
      """{
        |  val a = 0
        |  val a = 1
        |  a
        |}
        |""".stripMargin
    test(program, VDouble(0))
  }

}
