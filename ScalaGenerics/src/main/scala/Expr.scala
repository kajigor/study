//sealed trait Expr[T]
//case class Num(i: Int) extends Expr[Int]
//case class Bool(b: Boolean) extends Expr[Boolean]
//case class Add(x: Expr[Int], y: Expr[Int]) extends Expr[Int]
//case class Mult(x: Expr[Int], y: Expr[Int]) extends Expr[Int]
//case class Eq[A](x: Expr[A], y: Expr[A]) extends Expr[Boolean]
//
//def eval[T](e: Expr[T]): T = e match {
//  case Num(i) => i
//  case Bool(b) => b
//  case Add(x,y) => eval(x)+eval(y)
//  case Mult(x,y) => eval(x)*eval(y)
//  case Eq(x,y) => eval(x) == eval(y)
//}
//
//val expr1 = Mult(Add(Num(1),Num(2)),Num(4))
//val expr2 = Num(12)
//val expr3 = Eq(expr1,expr2)
//
//def exprTest() =
//  println(eval(expr3))

sealed case class Pair[A, B](first: A, second: B)

enum Expr[T] {
  case IntLit(value: Int) extends Expr[Int]
  case BoolLit(value: Boolean) extends Expr[Boolean]
  case IfExpr( cond: Expr[Boolean],
               when: Expr[T],
               otherwise: Expr[T],
             )
  case MkPair[A,B](fst: Expr[A], snd: Expr[B]) extends Expr[Pair[A,B]]
  case Fst[A, B](expr: Expr[Pair[A, B]]) extends Expr[A]
  case Snd[A, B](expr: Expr[Pair[A, B]]) extends Expr[B]
}

def eval[T](e: Expr[T]): T = e match {
  case Expr.IntLit(i) => i
  case Expr.BoolLit(b) => b
  case Expr.IfExpr(c, t, e) => if eval(c) then eval(t) else eval(e)
  case Expr.MkPair(f, s) => Pair(eval(f), eval(s))
  case Expr.Fst(e) => eval(e).first
  case Expr.Snd(e) => eval(e).second
}

def exprTest() =
  val pair = Expr.MkPair(Expr.IntLit(42), Expr.BoolLit(false))
  val expr = Expr.IfExpr(Expr.BoolLit(true), Expr.MkPair(Expr.Fst(pair), Expr.BoolLit(true)), Expr.MkPair(Expr.IntLit(13), Expr.Snd(pair)))
  val res = eval(expr)
  println(expr.toString())
  println(res)