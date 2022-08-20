package dentaku

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  private def expr: Parser[Expr] =
    term ~ rep(("+" | "-") ~ term) ^^ { case lhs ~ rhss =>
      rhss.foldLeft(lhs) { case (expr, op ~ rhs) =>
        op match {
          case "+" => Expr.Add(expr, rhs)
          case "-" => Expr.Minus(expr, rhs)
          case _   => throw new Exception
        }
      }
    }

  private def term: Parser[Expr] =
    factor ~ rep(("*" | "/") ~ factor) ^^ { case lhs ~ rhss =>
      rhss.foldLeft(lhs) { case (expr, op ~ rhs) =>
        op match {
          case "*" => Expr.Multi(expr, rhs)
          case "/" => Expr.Div(expr, rhs)
          case _   => throw new Exception
        }
      }
    }

  private def factor: Parser[Expr] = (
    floatingPointNumber ^^ (s => Expr.Const(s.toDouble))
      | "(" ~> expr <~ ")"
  )

  final def run(input: String): ParseResult[Expr] = parseAll(expr, input)

}
