package lang3

import org.scalatest.flatspec.AnyFlatSpec

class ParserTest extends AnyFlatSpec {

  it should "parse 1" in {
    val input =
      """{
        |  val a = 1 + 2 * 3
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        Assign(
          "a",
          Add(VDouble(1), Multi(VDouble(2), VDouble(3)))
        )
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse 2" in {
    val input =
      """{
        |  if (1 >= 2) { a } else { b }
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        IfExpr(
          expr = Gte(VDouble(1), VDouble(2)),
          onTrue = Block(Call("a")),
          onFalse = Block(Call("b"))
        )
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse 2-2" in {
    val input =
      """{
        |  if (a >= b) { a } else { b }
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        IfExpr(
          expr = Gte(Call("a"), Call("b")),
          onTrue = Block(Call("a")),
          onFalse = Block(Call("b"))
        )
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse 2-3-2" in {
    val input =
      """{
        |  val a = 0
        |  (1 + 2)
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        Assign(name = "a", ast = VDouble(0.0)),
        Add(VDouble(1), VDouble(2))
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse 2-4 ?" ignore {
    val input =
      """{
        |  if ({
        |    val a = 0
        |    (1 + 2)
        |  } >= {b}) 0 else b
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        IfExpr(
          expr = Lte(
            lhs = Block(
              Assign(name = "a", ast = VDouble(0.0)),
              Add(VDouble(1.0), VDouble(2.0))
            ),
            rhs = Block(Call("b"))
          ),
          onTrue = VDouble(0),
          onFalse = Call("b")
        )
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse callDef" in {
    val input =
      """{
        |  val b = max(a, 8)
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        Assign("b", CallFun("max", List(Call("a"), VDouble(8))))
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

  it should "parse 3" in {
    val input =
      """{
        |  val a = 1 + 2 * 3
        |  function max(a, b) {
        |    if (a >= b) a else b
        |  }
        |  println(a)
        |  val b = max(a, 8)
        |  println(b)
        |}
        |""".stripMargin

    val expected = {
      import AST.*
      Block(
        Assign(
          name = "a",
          ast = Add(VDouble(1), Multi(VDouble(2), VDouble(3)))
        ),
        DefFun(
          name = "max",
          args = List("a", "b"),
          body = IfExpr(
            expr = Gte(Call("a"), Call("b")),
            onTrue = Call("a"),
            onFalse = Call("b")
          )
        ),
        CallFun("println", List(Call("a"))),
        Assign("b", CallFun("max", List(Call("a"), VDouble(8)))),
        CallFun("println", List(Call("b")))
      )
    }

    println(Parser.run(input))

    val result = Parser.run(input).get

    assert(result == expected)
  }

}
