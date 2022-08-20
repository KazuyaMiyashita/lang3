package lang3

sealed trait AST

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
object AST {

  case class Block(asts: AST*) extends AST

  case class Assign(name: String, value: AST) extends AST
  case class DefFun(name: String, args: Seq[String], body: AST) extends AST

  sealed trait Expr extends AST
  case class IfExpr(expr: AST, onTrue: AST, onFalse: AST) extends Expr
  case class Call(name: String) extends Expr
  case class CallFun(name: String, args: Seq[AST]) extends Expr

  sealed trait ArithExpr extends Expr

  case class Add(lhs: AST, rhs: AST) extends ArithExpr
  case class Minus(lhs: AST, rhs: AST) extends ArithExpr
  case class Multi(lhs: AST, rhs: AST) extends ArithExpr
  case class Div(lhs: AST, rhs: AST) extends ArithExpr
  case class Eq(lhs: AST, rhs: AST) extends ArithExpr
  case class Gt(lhs: AST, rhs: AST) extends ArithExpr
  case class Gte(lhs: AST, rhs: AST) extends ArithExpr
  case class Lt(lhs: AST, rhs: AST) extends ArithExpr
  case class Lte(lhs: AST, rhs: AST) extends ArithExpr

  sealed trait Literal extends ArithExpr

  case class VDouble(value: Double) extends Literal
  case class VBool(value: Boolean) extends Literal

}
