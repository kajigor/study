trait Nat
class _0 extends Nat
class Succ[N <: Nat] extends Nat

type _1 = Succ[_0]
type _2 = Succ[_1] // Succ[Succ[_0]]
type _3 = Succ[_2]
type _4 = Succ[_3]
type _5 = Succ[_4]

// name it "LessThan", then refactor to "<"
trait <[A <: Nat, B <: Nat]
object < {
  given basic[B <: Nat]: <[_0, Succ[B]] with {}
  given inductive[A <: Nat, B <: Nat](using lt: <[A, B]): <[Succ[A], Succ[B]] with {}
  def apply[A <: Nat, B <: Nat](using lt: <[A, B]) = lt
}
val comparison = <[_1, _2]
//No given instance of type _1 < _0 was found for parameter lt of method apply in object <

/*
  <.apply[_1, _3] -> requires <[_1, _3]
  inductive[_0, _2] -> requires <[_0, _2]
  ltBasic[_1] -> produces <[_0, Succ[_1]] == <[_0, _2]
 */

trait <=[A <: Nat, B <: Nat]
object <= {
  given lteBasic[B <: Nat]: <=[_0, B] with {}
  given inductive[A <: Nat, B <: Nat](using lte: <=[A, B]): <=[Succ[A], Succ[B]] with {}
  def apply[A <: Nat, B <: Nat](using lte: <=[A, B]) = lte
}
val lteTest: _1 <= _1 = <=[_1, _1]

def testPeano() =
  println(comparison)
  println(lteTest)