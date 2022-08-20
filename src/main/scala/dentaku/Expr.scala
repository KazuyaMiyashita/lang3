package dentaku

sealed trait Expr

object Expr {
  case class Const(value: Double) extends Expr

  case class Add(lhs: Expr, rhs: Expr) extends Expr

  case class Minus(lhs: Expr, rhs: Expr) extends Expr

  case class Multi(lhs: Expr, rhs: Expr) extends Expr

  case class Div(lhs: Expr, rhs: Expr) extends Expr
}
