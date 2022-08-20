package dentaku

import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {

  test("calc 1") {
    assert(Interpreter.run(Expr.Const(1.0)) == 1.0)
  }

}
