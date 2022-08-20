package lang3

import org.scalatest.flatspec.AnyFlatSpec

class InterpreterTest extends AnyFlatSpec {

  it should "run" in {
    /*
    {
      val a = 1 + 2 * 3
      function max(a, b) {
        if (a >= b) a else b
      }
      println(a)
      val b = max(a, 8)
      println(b)
    }
     */
    val ast = {
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

    val expected = AST.VUnit
    val result = Interpreter.run(ast)

    assert(result == expected)

  }

}
