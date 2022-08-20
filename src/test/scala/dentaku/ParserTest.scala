package dentaku

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("parse 1") {
    assert(Parser.run("1").get == Expr.Const(1))
  }

  test("parse 1 + 2") {
    val input = "1 + 2"
    val expected = Expr.Add(
      Expr.Const(1),
      Expr.Const(2)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 - 2") {
    val input = "1 - 2"
    val expected = Expr.Minus(
      Expr.Const(1),
      Expr.Const(2)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 * 2") {
    val input = "1 * 2"
    val expected = Expr.Multi(
      Expr.Const(1),
      Expr.Const(2)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 / 2") {
    val input = "1 / 2"
    val expected = Expr.Div(
      Expr.Const(1),
      Expr.Const(2)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 + 2 + 3 to add(add(1 + 2) + 3)") {
    val input = "1 + 2 + 3"
    val expected = Expr.Add(
      Expr.Add(
        Expr.Const(1),
        Expr.Const(2)
      ),
      Expr.Const(3)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 + 2 * 3 to add(1 + mul(2 + 3))") {
    val input = "1 + 2 * 3"
    val expected = Expr.Add(
      Expr.Const(1),
      Expr.Multi(
        Expr.Const(2),
        Expr.Const(3)
      )
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse (1 + 2) * 3 to mul(add(1 + 2), 3)") {
    val input = "(1 + 2) * 3"
    val expected = Expr.Multi(
      Expr.Add(
        Expr.Const(1),
        Expr.Const(2)
      ),
      Expr.Const(3)
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 * 2 + 3 / 4 to add(mul(1, 2), div(3, 4))") {
    val input = "1 * 2 + 3 / 4"
    val expected = Expr.Add(
      Expr.Multi(
        Expr.Const(1),
        Expr.Const(2)
      ),
      Expr.Div(
        Expr.Const(3),
        Expr.Const(4)
      )
    )
    assert(Parser.run(input).get == expected)
  }

  test("parse 1 * (2 + 3) / 4 to mul(1, div(add(2, 3), 4))") {
    val input = "1 * 2 + 3 / 4"
    val expected = Expr.Multi(
      Expr.Const(1),
      Expr.Div(
        Expr.Add(
          Expr.Const(2),
          Expr.Const(3)
        ),
        Expr.Const(4)
      )
    )
    assert(Parser.run(input).get == expected)
  }
}
