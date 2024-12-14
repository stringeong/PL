package call_by_reference

sealed trait Expr
case class Num (value: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Val(x: String, i: Expr, b: Expr) extends Expr
case class Id(x: String) extends Expr
case class Fun(x: String, b: Expr) extends Expr
case class App(f: Expr, a: Expr) extends Expr
case class Set(x: String, e: Expr) extends Expr

type Addr = Int
var newAddr = 0
type Env = Map[String, Addr]
type Sto = Map[Addr, Value]

sealed trait Value
case class NumV (n: Int) extends Value
case class CloV (p: String, b: Expr, e: Env) extends Value

def interp (e: Expr, env: Env, s: Sto): (Value, Sto) = {
  e match {
    case Num(n) => (NumV(n), s)
    case Add(l, r) => {
      val (NumV(n), s_l) = interp(l, env, s)
      val (NumV(m), s_r) = interp(r, env, s_l)
      (NumV(n + m), s_r)
    }

    case Sub(l, r) => {
      val (NumV(n), s_l) = interp(l, env, s)
      val (NumV(m), s_r) = interp(r, env, s_l)
      (NumV(n - m), s_r)
    }

    case Id(x) => {
      (s(env(x)), s)
    }
    // val x = i in b
    case Val(x, i, b) => {
      val (v, s_i) = interp(i, env, s)
      val addr = newAddr
      newAddr += 1
      interp(b, env + (x -> addr), s_i + (addr -> v))
    }
    case Fun(x, b) => (CloV(x, b, env), s)
    case App(f, a) => {
      val (CloV(x, b, cEnv), s_f) = interp(f, env, s)
      a match {
        case Id(y) => { // 참조 전달
          val addr = env(y)
          interp(b, cEnv + (x -> addr), s_f)
        }
        case _ => { //값 전달
          val (v, s_a) = interp(a, env, s_f)
          val addr = newAddr
          newAddr += 1
          interp(b, cEnv + (x -> addr), s_a + (addr -> v))
        }
      }
    }
    case Set(x, e) => {
      val (v, m_1) = interp(e, env, s)
      val addr = env(x)
      (v, m_1 + (addr -> v))
    }
  }
}

@main
def main(): Unit = {
  val t0 =
    Val("a",
      Num(10),
        App(Fun("x", Set("x", Num(5))), Id("a")))
  val t = interp(t0, Map.empty, Map.empty)
  println(t)
}
