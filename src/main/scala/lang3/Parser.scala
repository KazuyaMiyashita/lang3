package lang3

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  import AST.*

  private val ReservedWords = List("val", "function", "if", "else")

  private def stringParser: Parser[String] =
    ("""[a-z]+""").r

  private def variableParser: Parser[String] = stringParser.filter(!ReservedWords.contains(_))

  object LiteralParsers {
    def literal: Parser[Literal] = (
      floatingPointNumber ^^ (s => VDouble(s.toDouble))
        | "true" ^^ (_ => VBool(true))
        | "false" ^^ (_ => VBool(false))
    )
  }

  object ExprParsers {

    def expr: Parser[Expr] = arithExpr

    def methodExpr: Parser[Expr] = ifExpr | callFun | call

    def ifExpr: Parser[IfExpr] =
      "if" ~ "(" ~ ast ~ ")" ~ ast ~ "else" ~ ast ^^ { case _ ~ _ ~ expr ~ _ ~ onTrue ~ _ ~ onFalse =>
        IfExpr(expr, onTrue, onFalse)
      }

    def callFun: Parser[CallFun] =
      variableParser ~ "(" ~ repsep(ast, ",") ~ ")" ^^ { case name ~ _ ~ args ~ _ =>
        CallFun(name, args)
      }

    def call: Parser[Call] =
      variableParser ^^ Call.apply

    def arithExpr: Parser[Expr] = priority3

    def priority3: Parser[Expr] =
      priority2 ~ rep(("==" | "<=" | "<" | ">=" | ">") ~ priority2) ^^ { case lhs ~ rhss =>
        rhss.foldLeft(lhs) { case (expr, op ~ rhs) =>
          op match {
            case "==" => Eq(expr, rhs)
            case "<=" => Lte(expr, rhs)
            case "<"  => Lt(expr, rhs)
            case ">=" => Gte(expr, rhs)
            case ">"  => Gt(expr, rhs)
            case _    => throw new Exception
          }
        }
      }

    def priority2: Parser[Expr] =
      priority1 ~ rep(("+" | "-") ~ priority1) ^^ { case lhs ~ rhss =>
        rhss.foldLeft(lhs) { case (expr, op ~ rhs) =>
          op match {
            case "+" => Add(expr, rhs)
            case "-" => Minus(expr, rhs)
            case _   => throw new Exception
          }
        }
      }

    def priority1: Parser[Expr] =
      priority0 ~ rep(("*" | "/") ~ priority0) ^^ { case lhs ~ rhss =>
        rhss.foldLeft(lhs) { case (expr, op ~ rhs) =>
          op match {
            case "*" => Multi(expr, rhs)
            case "/" => Div(expr, rhs)
            case _   => throw new Exception
          }
        }
      }

    def priority0: Parser[Expr] =
      LiteralParsers.literal | methodExpr | "(" ~> expr <~ ")"

  }

  private def assignParser: Parser[Assign] =
    "val" ~ variableParser ~ "=" ~ ast ^^ { case _ ~ name ~ _ ~ ast =>
      Assign(name, ast)
    }

  private def defFunParser: Parser[DefFun] =
    "function" ~ variableParser ~ "(" ~ repsep(variableParser, ",") ~ ")" ~ "{" ~ ast ~ "}" ^^ {
      case _ ~ name ~ _ ~ args ~ _ ~ _ ~ body ~ _ =>
        DefFun(name, args, body)
    }

  private def block: Parser[Block] =
    "{" ~> rep(ExprParsers.expr | assignParser | defFunParser) <~ "}" ^^ (asts => Block(asts: _*))

  private def ast: Parser[AST] =
    ExprParsers.expr | block

  final def run(input: String): ParseResult[AST] = parseAll(ast, input)

}
