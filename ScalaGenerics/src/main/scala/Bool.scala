//sealed trait Bool {
//  type If[T <: Up, F <: Up, Up] <: Up
//}
//
//sealed trait True extends Bool {
//  type If[T <: Up, F <: Up, Up] = T
//}
//sealed trait False extends Bool {
//  type If[T <: Up, F <: Up, Up] = F
//}
//
////scala> type Rep[A <: Bool] = A#If[ Int, Long, AnyVal ]
////defined type alias Rep
////
////scala> implicitly[ Rep[True] =:= Int ]
////res1: =:=[Rep[Booleans.True],Int] = <function1>
////
////  scala> implicitly[ Rep[False] =:= Int ]
////  error: could not find implicit value for parameter e: =:=[Rep[Booleans.False],Int]
////
////  scala> implicitly[ Rep[False] =:= Long ]
////  res3: =:=[Rep[Booleans.False],Long] = <function1>
//
//object Bool {
//  type And[A <: Bool, B <: Bool] = A#If[B, False, Bool]
//  type Or[A <: Bool, B <: Bool] = A#If[True, B, Bool]
//  type Not[A <: Bool] = A#If[False, True, Bool]
//}
//
//
////scala > implicitly[True && False || Not[False] =:= True]
//
//def toBoolean[B <: Bool](implicit b: BoolRep[B]): Boolean = b.value
//
//class BoolRep[B <: Bool](val value: Boolean)
//implicit val falseRep: BoolRep[False] = new BoolRep(false)
//implicit val trueRep: BoolRep[True] = new BoolRep(true)
//
////scala> toBoolean[ True && False || Not[False] ]
////res0: Boolean = true
//
//def testBool() =
//  toBoolean[And[True, Or [False, Not[False]]]]