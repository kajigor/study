enum EQ[S, T] {
  case Refl[U]() extends EQ[U, U]
}

enum SUB[-S, +T] {
  case Refl[U]() extends SUB[U, U]
}

def convert[T, T2](t: T, ev: SUB[T, T2]): Unit =
  ev match {
    case e: SUB.Refl[_] =>
      val b: T2 = t: T
  }

def convertTest =
  convert[Int, Int](42, SUB.Refl())

//  def convert[T, T2](t: T, ev: SUB[T, T2]): T2 =
//    ev match {
//      case e: SUB.Refl[_] => e
//    }
// // Found: (e : SUB.Refl[_]) Required: Int
// // where: _ is a type in method convert with bounds >: T and <: Int