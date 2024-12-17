package BFAE

sealed trait Expr
case class Num (value: Int) extends Expr
case class Add (left: Expr, right: Expr) extends Expr
case class Sub (left: Expr, right: Expr) extends Expr
case class Val (x: String, i: Expr, b: Expr) extends Expr
case class Id (x: String) extends Expr
case class Fun (x: String, b: Expr) extends Expr
case class App (f: Expr, a: Expr) extends Expr
case class Ifz (c: Expr, t: Expr, f: Expr) extends Expr
case class Def (f: String, x: String, b: Expr, e: Expr) extends Expr

case class NewBox (e: Expr) extends Expr // box e
case class OpenBox (e: Expr) extends Expr // !e
case class SetBox (e1: Expr, e2: Expr) extends Expr// e:=e
case class Seqn (e1: Expr, e2: Expr ) extends Expr //e ; e

sealed trait Value
case class NumV (n: Int) extends Value
case class CloV (x: String, b: Expr, var env: Env) extends Value
case class AddrV (a: Addr) extends Value

type Addr = Int
var nextAddr = 0
type Env = Map[String, Value] //Id --> Value
type Sto = Map[Addr, Value] //Addr --> Value, 실행 순서에 따라 흘러감


// => \subseteq Expr X Env X V //RFAE
// => \sunseteq Expr X Env X Sto X V X Sto //BFAE
// => : Expr X Env X Sto -> V X Sto
def interp (e: Expr, env: Env, s: Sto): (Value, Sto)= {
  e match {
    case Num(n) => (NumV(n), s) //받은 s 그대로 넘겨줌
    case Id(x) => (env(x), s)
    case Fun(x, b) => (CloV(x, b, env), s)

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

    // val x = i in b
    case Val(x, i, b) => {
      val (v, s_i) = interp(i, env, s)
      interp(b, env + (x -> v), s_i)
    }

    case App(f, a) => {
      val (CloV(x, b, cEnv),s_f) = interp(f, env, s)
      val (v, s_a) = interp(a, env, s_f)
      interp(b, cEnv + (x -> v), s_f)
    }

    case Ifz(c, t, f) => {
      val (v, s_c) = interp(c, env, s)

      if(v == Num(0))
        interp(t, env, s_c)
      else
        interp(f, env, s_c)
    }

    case Def(f, x, b, e) => {
      val clos = CloV(x, b, env)
      val nenv = env + (f -> clos)
      clos.env = nenv

      interp(e, nenv,s)
    }

    case NewBox (e: Expr) => {
      val (v, m_1) = interp(e, env, s)
      val a = nextAddr
      nextAddr = nextAddr + 1
      (AddrV(a), m_1 + (a -> v))
    }

    case OpenBox (e: Expr) => {
      val (AddrV(a), m_1) = interp(e, env, s)
      (m_1(a) ,m_1)
    }

    case SetBox (e1: Expr, e2: Expr) => {
      val (AddrV(a), m_1) = interp(e1, env, s)
      val (v, m_2) = interp(e2, env, m_1)
      (v, m_2 + (a -> v))
    }

    case Seqn (l, r) => {
      val (_, s_l) = interp(l, env, s)
      interp(r, env, s_l)
    }
  }
}

@main
def main (): Unit = {
  val p =
    interp(
      App(
        Fun("x",
          Seqn(
            SetBox(Id("x"), Num(3)),
            OpenBox(Id("x"))
          )
        ),
        NewBox(Num(2))
      ),
      Map.empty,
      Map.empty)

  //(λx.(λy.x:=8; !y) x) box 7
  val t0 = NewBox(Num(7)) // box 7
  val t1 = Fun("x", // λx
    App(
      Fun("y", // λy
        Seqn(
          SetBox(Id("x"), Num(8)), // x := 8
          OpenBox(Id("y")) // !y
        )
      ),
      Id("x") // y에 x를 전달
    )
  )
  val t3 = App(t1, t0) // λx 실행, x = box 7
  
  println(p)
  println(t3)
}
