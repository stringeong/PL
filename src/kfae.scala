package KFAE

sealed trait Expr
case class Num (value: Int) extends Expr
case class Add (left: Expr, right: Expr) extends Expr
case class Sub (left: Expr, right: Expr) extends Expr
case class Val (x: String, i: Expr, b: Expr) extends Expr
case class Id (x: String) extends Expr
case class Fun (x: String, b: Expr) extends Expr
case class App (f: Expr, a: Expr) extends Expr
case class Vcc(x: String, b: Expr) extends Expr

type Env = Map[String, Value]
type Cont = Value => Value

sealed trait Value
case class NumV (n: Int) extends Value
case class CloV (p: String, b: Expr, e: Env) extends Value
case class ContV(k: Cont) extends Value

def numVAdd(v1: Value, v2: Value): Value = {
  val NumV(n1) = v1
  val NumV(n2) = v2
  NumV(n1 + n2)
}
def numVSub(v1: Value, v2: Value): Value = {
  val NumV(n1) = v1
  val NumV(n2) = v2
  NumV(n1 - n2)
}

def interp (e: Expr, env: Env, k: Cont): Value = {
  e match {
    case Num(n) => k(NumV(n))
    
    case Add(e1, e2) =>
      interp(e1, env, v1 =>
        interp(e2, env, v2 =>
          k(numVAdd(v1, v2))))
      
    case Sub(e1, e2) =>
      interp(e1, env, v1 =>
        interp(e2, env, v2 =>
          k(numVSub(v1, v2))))
      
    case Id(x) => k(env(x))
    case Fun(x, b) => k(CloV(x, b, env))
    case App(e1, e2) =>
      interp(e1, env, v1 =>
        interp(e2, env, v2 => v1 match {
          case CloV(xv1, ev1, sigmav1) =>
            interp(ev1, sigmav1 + (xv1 -> v2), k)
          case ContV(k) => k(v2)
        })
      )
    case Vcc(x, b) =>
      interp(b, env + (x -> ContV(k)), k)
  }
}

@main
def main (): Unit = {
  // 1 + (vcc x in ((x 2) + 3))
  interp(
    Add(
      Num(1),
      Vcc("x",
        Add(
          App(Id("x"), Num(2)),
          Num(3)
        )
      )
    ),
    Map.empty,
    x => x
  )
}


