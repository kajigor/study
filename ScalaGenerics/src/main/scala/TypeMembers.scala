trait Input {
  type Output
  val value: Output
}

def dependentFunc(i: Input): i.Output = i.value

def valueOf[T](v: T) = new Input {
  type Output = T
  val value: T = v
}

// // What is Output type in the input?
//def valueOfT(v: Output) = new Input {
//  type Output
//  val value: Output = v
//}

val intValue    = valueOf(1)
val stringValue = valueOf("One")

def checkStuff =
  assert(dependentFunc(intValue) == 1)
  assert(dependentFunc(stringValue) == "One")

// same but with generics
trait InputG[Output] {
  val value: Output
}

def dependentFuncG[O](i: InputG[O]): O = i.value

def valueOfG[T](v: T) = new InputG[T] {
  val value: T = v
}

val intValueG    = valueOfG(1)
val stringValueG = valueOfG("One")

def checkStuffG =
  assert(dependentFuncG(intValueG) == 1)
  assert(dependentFuncG(stringValueG) == "One")

// Path dependency:
class Foo {
  class Bar
}

val f1 = new Foo
val b1: f1.Bar = new f1.Bar
val f2 = new Foo
val b2: f2.Bar = new f2.Bar
def check = assert(b1 != b2)

def typeMembersTest() =
  checkStuff
  checkStuffG
  check 
  


