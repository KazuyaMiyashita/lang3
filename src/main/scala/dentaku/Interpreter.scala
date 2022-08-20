package dentaku

object Interpreter {

  def run(expr: Expr): Double = expr match {
    case Expr.Const(value)    => value
    case Expr.Add(lhs, rhs)   => run(lhs) + run(rhs)
    case Expr.Minus(lhs, rhs) => run(lhs) - run(rhs)
    case Expr.Multi(lhs, rhs) => run(lhs) * run(rhs)
    case Expr.Div(lhs, rhs)   => run(lhs) / run(rhs)
  }

}
