package LFAE // call by name

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
 
    val t = App(Fun("x", Num(1)), App(Num(1), Num(1))) 
    println(interp(t, Map.empty))
    /*
    lambda x. 1 (1 1) -> FAE라면 1 1 에서 오류 (1이 클로저가 아니기에) but 함수 body엔 x가 등장하지 않으므로 1 출력
    call by neme은, 매개변수가 함수 body에서 적게 사용될 땐 효율적이지만, 같은 매개변수가 두 번 이상 사용되면 반복된 계산 >> 비효율적
    */
    //예시
    Fun("g", Add(Id("y"), Num(1))) // g(y) = y + 1
    val t1 = App(Fun("f", Add(Id("x"), Id("x"))), Id("g"))
  // f(x) = x + x 일 때, f(g) = g + g 즉, g가 2번 평가됨 + Add(Id("x"), Id("x))에서 strict interp overhead 발생
  // call by need 로 memoization을 이용해 최적화 가능
    println(interp(t1, Map.empty))
  
  
   

}
