case class Parent(name: String) {
  class Child

  def child = new this.Child

  def punish(c: this.Child): Unit =
    println(s"$name is punishing $c")

  def reward(c: Parent#Child): Unit =
    println(s"$name is rewarding $c")
}

def punishmentTest() =
  val john = Parent("John")
  val scarlet = Parent("Scarlet")

  john.punish(john.child)
//  john.punish(scarlet.child) //Compile time error