package LFAE

sealed trait Expr
case class Num (value: Int) extends Expr
case class Add (left: Expr, right: Expr) extends Expr
case class Sub (left: Expr, right: Expr) extends Expr
case class Val (x: String, i: Expr, b: Expr) extends Expr
case class Id (x: String) extends Expr
case class Fun (x: String, b: Expr) extends Expr
case class App (f: Expr, a: Expr) extends Expr

type Env = Map[String, Value]

sealed trait Value
case class NumV (n: Int) extends Value
case class CloV (p: String, b: Expr, e: Env) extends Value
case class ExprV (e: Expr, env: Env) extends Value

def strict (v: Value): Value = v match {
  case ExprV (e, env) => strict(interp(e, env))
  case _ => v
} //infinite loop 고려 x (side effect)

def interp (e: Expr, env: Env): Value = {
  e match {
    case Num(n) => NumV(n)
    case Add(l, r) => {
      val NumV(n) = strict(interp(l, env))
      val NumV(m) = strict(interp(r, env))

      NumV(n + m)
    }
    case Sub(l, r) => {
      val NumV(n) = strict(interp(l, env))
      val NumV(m) = strict(interp(r, env))

      NumV(n - m)
    }
    case Val(x, i, b) => {
      interp(b, env + (x -> interp(i, env)))
    }
    case Id(x) => env(x)
    case Fun(x, b) => CloV(x, b, env)
    case App(f, a) => {
      val CloV(x, b, cEnv) = strict(interp(f, env))
      interp(b, cEnv + (x -> ExprV(a, env))) // argument를 넘길 때, ExprV로 넘김
    }
  }
}

@main
def main (): Unit = {
  interp(
    App(
      Fun("x", Num(1)),
      App(Num(1), Num(1))
    ),
    Map.empty
  )

}
