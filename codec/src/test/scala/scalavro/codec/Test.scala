package scalavro.codec

import org.scalatest.{FlatSpec, Matchers}
import scodec.Codec

class Test extends FlatSpec with Matchers {

  case class Address(number: Int, pcde: String)

  case class Person(name: String, age: Int, addr: Address)

  "Case Classes" should "just werk" in {
    val tom = Person("tom", 26, Address(1, "AAA S60"))

    val codec = Codec[Person]
    val bv = codec.encode(tom)
    val result = codec.decode(bv.require).require.value
    result shouldEqual tom
  }

}