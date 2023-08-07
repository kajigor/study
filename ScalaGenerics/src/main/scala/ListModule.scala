class EmptyListException(s: String) extends Exception(s) {}

object ListModule {
  trait List { self =>
    type Elem
    def head(): Elem
    def tail(): List & { type Elem <: self.Elem }
  }

  def nil() = new List {
    type Elem = Nothing // <: T
    def head() = throw EmptyListException("Cannot get head of an empty list")
    def tail() = throw EmptyListException("Cannot get tail of an empty list")
  }

  def cons[T](hd: T, tl: List { type Elem <: T }): List { type Elem = T } =
    new List {
      type Elem = T
      def head() = hd
      def tail() = tl
    }

  def map[A, B](f: A => B, lst: List { type Elem <: A }): List { type Elem = B } =
    new List {
      type Elem = B
      def head() = f(lst.head())
      def tail() = map(f, lst.tail())
    }
}

object ListModuleNoGenerics {
  trait List { self =>
    type Elem
    def head(): Elem
    def tail(): List & { type Elem <: self.Elem }
  }

  def nil() = new List {
    type Elem = Nothing
    def head() = throw EmptyListException("Cannot get head of an empty list")
    def tail() = throw EmptyListException("Cannot get tail of an empty list")
  }

  def cons(t: { type A }) (hd: t.A) (tl: List { type Elem <: t.A }) =
    new List {
      type Elem = t.A
      def head() = hd
      def tail() = tl
    }

  def map(a: { type T }) (b: { type T }) (f: a.T => b.T) (lst: List { type Elem <: a.T }): List { type Elem = b.T } =
    new List {
      type Elem = b.T
      def head() = f(lst.head())
      def tail() = map(a)(b)(f)(lst.tail())
    }
}

def listModuleTest() =
  import ListModule._

  val list1: List {type Elem = Int} = cons(1, nil())
  val list2 = cons("Hello", cons("World!", nil()))
  val list3 = map(x => 42, list2)
  val list4 = map[String, Int](x => x.length, list2)

  val mapStringsToInts = map[String, Int]
  val list5 = mapStringsToInts(x => x.length * 42, list2)

  println("\nlistModuleTest")
  println(list1.head()) // Output: 1
  println(list2.head()) // Output: Hello
  println(list2.tail().head()) // Output: World!
  println(list3.head()) // Output: 42
  println(list3.tail().head()) // Output: 42
  println(list4.head()) // Output: 5
  println(list4.tail().head()) // Output: 6
  println(list5.head()) // Output: 210
  println(list5.tail().head()) // Output: 252

def listModuleNoGenericsTest() =
  import ListModuleNoGenerics._

  // // won't work -- use () () ()
  // val list1 = cons (new IntType, 1, nil())

  // // won't work -- use class and its instance
//   val list1 = cons (new { type A = Int }) (1) (nil())

  class IntType { type A = Int }
  val intTypeObject = new IntType
  class StringType { type A = String }
  val stringTypeObject = new StringType

  val list1 = cons (new IntType) (1) (nil())
  val list2 = cons (new StringType) ("Hello") (cons(stringTypeObject) ("World!") (nil()))

  class IntTypeT {
    type T = Int
  }
  val intTypeObjectT = new IntTypeT

  class StringTypeT {
    type T = String
  }
  val stringTypeObjectT = new StringTypeT

  val list3 = map (stringTypeObjectT) (intTypeObjectT) (x => 42) (list2)
  val list4 = map (stringTypeObjectT) (intTypeObjectT) (x => x.length) (list2)

  val mapStringsToInts = map (stringTypeObjectT) (intTypeObjectT)

  val list5 = mapStringsToInts(x => x.length * 42) (list2)

  println("\nlistModuleNoGenericsTest")
  println(list1.head()) // Output: 1
  println(list2.head()) // Output: Hello
  println(list2.tail().head()) // Output: World!
  println(list3.head()) // Output: 42
  println(list3.tail().head()) // Output: 42
  println(list4.head()) // Output: 5
  println(list4.tail().head()) // Output: 6
  println(list5.head()) // Output: 210
  println(list5.tail().head()) // Output: 252

def listTest() =
  listModuleTest()
  listModuleNoGenericsTest()
