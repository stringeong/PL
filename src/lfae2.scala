package LFAE2 // Call by Need

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
case class ExprV (e: Expr, env: Env, var v : Option[Value] = None) extends Value //enumerator 기본 값을 None으로 초기화

def strict (v: Value): Value = v match { //helper function
  case ev @ ExprV(e, env, None) => { //ev @: pattern matching에서 ev를 사용하여 필드 참조할 때 쓰임
    val cache = strict(interp(e, env))
    ev.v = Some(cache)
    cache
  }
  case ExprV(_, _, Some(cache)) => cache
  case _ => v
}

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
      interp(b, cEnv + (x -> ExprV(a, env)))
    }
  }
}

@main
def main (): Unit = {
  val t0 = App(Fun("y", Add(Id("y"), Num(1))), Num(2))// lambda y. y + 1 (2)
  val t1 = App(Fun("x", Add(Id("x"), Id("x"))), t0) // lambda x. x + x (y)
  val t2 = App(Fun("x", Add(Id("x"), Id("x"))), App(Fun("y", Add(Id("y"), Num(1))), Num(2)))
  // lambda x. x + x ( lambda y. y + 1 (2)) 즉, t1과 t2는 같은 표현
  println(interp(t2, Map.empty))
}
