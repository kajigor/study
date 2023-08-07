trait HasTail:
  def tailColor: String
  def wagTail() = println(s"$tailColor tail is wagging")
  def stopTail() = println("Tail is stopped")

trait HasLegs:
  def numLegs: Int
  def walk() = println("I'm walking")
  def stop() = println("Stopped walking")

class Dog(name: String, colorTail: String = "Red") extends HasLegs, HasTail:
  val numLegs = 4
  val tailColor = colorTail
  override def walk() = println(s"$name is walking with its $numLegs legs")
  override def toString() = s"$name is a Dog"

def dogTest() =
  val dog = Dog("Fluffy")
  val secondDog = Dog("Speedy", "White")
  dog.walk()
  secondDog.walk()
  dog.wagTail()
  secondDog.wagTail()
