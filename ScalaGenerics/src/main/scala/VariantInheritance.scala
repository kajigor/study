trait Base[+T]
class Derived1 extends Base[Any]
class Derived2 extends Derived1, Base[String]
// wouldn't work with invariant Base; would work with co- and contravariant Base
