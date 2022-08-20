package lang3

object Interpreter {

  import AST.*

  def run(ast: AST): AST.Literal = {
    val environment: Map[String, Any] = Map.empty
    runImpl(ast, environment)._1
  }

  def runImpl(ast: AST, env: Map[String, Any]): (AST.Literal, Map[String, Any]) = ast match {
    case Block(asts @ _*) =>
      val (res, _) = asts.foldLeft((VUnit: Literal, env)) { case ((_, lastEnv), nextAst) =>
        runImpl(nextAst, lastEnv)
      }
      (res, env)
    case Assign(name, ast) =>
      (VUnit, env + (name -> runImpl(ast, env)._1))
    case defFun @ DefFun(name, _, _) =>
      (VUnit, env + (name -> defFun))
    case IfExpr(expr, onTrue, onFalse) =>
      runImpl(expr, env)._1 match {
        case VBool(value) =>
          if (value) { runImpl(onTrue, env) }
          else { runImpl(onFalse, env) }
        case _ => throw new TypeMismatch
      }
    case Call(name) =>
      env.get(name) match {
        case Some(value) =>
          value match {
            case literal: Literal => (literal, env)
            case _                => throw new VariableNotLiteral(name)
          }
        case None => throw new VariableNotFound(name)
      }
    case CallFun(name, args) =>
      name match {
        case "println" =>
          if (args.length == 1) {
            val value = runImpl(args.head, env)._1
            value match {
              case VDouble(value) => println(value)
              case VBool(value)   => println(value)
              case AST.VUnit      => println(())
            }
            (VUnit, env)
          } else throw new ArgsLengthMismatch
        case _ =>
          env.get(name) match {
            case Some(value) =>
              value match {
                case defFun: DefFun =>
                  if (args.length == defFun.args.length) {
                    val values: Seq[Literal] = args.map { arg =>
                      runImpl(arg, env)._1
                    }
                    runImpl(defFun.body, env ++ (defFun.args.zip(values)))
                  } else throw new ArgsLengthMismatch
                case _ => throw new VariableNotFunction(name)
              }
            case None => throw new FunctionNotFound(name)
          }
      }
    case Add(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VDouble(l + r), env)
        case _                                  => throw new TypeMismatch
      }
    case Minus(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VDouble(l - r), env)
        case _                                  => throw new TypeMismatch
      }
    case Multi(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VDouble(l * r), env)
        case _                                  => throw new TypeMismatch
      }
    case Div(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VDouble(l / r), env)
        case _                                  => throw new TypeMismatch
      }
    case Eq(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VBool(l == r), env)
        case _                              => throw new TypeMismatch
      }
    case Gt(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VBool(l > r), env)
        case _                              => throw new TypeMismatch
      }
    case Gte(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VBool(l >= r), env)
        case _                              => throw new TypeMismatch
      }
    case Lt(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VBool(l < r), env)
        case _                              => throw new TypeMismatch
      }
    case Lte(lhs, rhs) =>
      (runImpl(lhs, env), runImpl(rhs, env)) match {
        case ((VDouble(l), _), (VDouble(r), _)) => (VBool(l <= r), env)
        case _                              => throw new TypeMismatch
      }
    case v: VDouble => (v, env)
    case v: VBool   => (v, env)
    case VUnit      => (VUnit, env)

  }

  class TypeMismatch extends RuntimeException("type mismatch!")
  class VariableNotFound(name: String) extends RuntimeException(s"variable $name not found!")
  class VariableNotLiteral(name: String) extends RuntimeException(s"variable $name not literal!")
  class FunctionNotFound(name: String) extends RuntimeException(s"function $name not found!")
  class VariableNotFunction(name: String) extends RuntimeException(s"variable $name not function!")
  class ArgsLengthMismatch extends RuntimeException("argument length mismatch!")

}
