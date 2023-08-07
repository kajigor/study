// from https://www.baeldung.com/scala/path-dependent-types#:~:text=Scala%20has%20a%20notion%20of,rather%20in%20the%20type%20placement.

import scala.collection.mutable
import java.nio.ByteBuffer


abstract class Key(val name: String) {
  type ValueType
}

trait Operations {
  def set(key: Key)(value: key.ValueType)(implicit enc: Encoder[key.ValueType]): Unit
  def get(key: Key)(implicit decoder: Decoder[key.ValueType]): Option[key.ValueType]
}

case class Database() extends Operations {

  private val db = mutable.Map.empty[String, Array[Byte]]

  def set(k: Key)(v: k.ValueType)(implicit enc: Encoder[k.ValueType]): Unit =
    db.update(k.name, enc.encode(v))

  def get(
           k: Key
         )(implicit decoder: Decoder[k.ValueType]): Option[k.ValueType] = {
    db.get(k.name).map(x => decoder.encode(x))
  }

}

object Database {
  def key[Data](v: String) =
    new Key(v) {
      override type ValueType = Data
    }
}

trait Encoder[T] {
  def encode(t: T): Array[Byte]
}

object Encoder {
  implicit val stringEncoder: Encoder[String] = new Encoder[String] {
    override def encode(t: String): Array[Byte] = t.getBytes
  }

  implicit val doubleEncoder: Encoder[Double] = new Encoder[Double] {
    override def encode(t: Double): Array[Byte] = {
      val bytes = new Array[Byte](8)
      ByteBuffer.wrap(bytes).putDouble(t)
      bytes
    }
  }
}

trait Decoder[T] {
  def encode(d: Array[Byte]): T
}

object Decoder {
  implicit val stringDecoder: Decoder[String] = (d: Array[Byte]) =>
    new String(d)
  implicit val intDecoder: Decoder[Double] = (d: Array[Byte]) =>
    ByteBuffer.wrap(d).getDouble
}

def databaseTest() =
  val db = Database()
  import Database._
  val k1 = key[String]("key1")
  val k2 = key[Double]("key2")

  db.set(k1)("One")
  db.set(k2)(1.0)
  assert(db.get(k1).contains("One"))
  assert(db.get(k2).contains(1.0))