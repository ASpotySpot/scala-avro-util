
package scalavro.macros_test

import org.apache.avro.generic.{GenericContainer, IndexedRecord}
import org.scalatest.{FlatSpec, Matchers}
import scalavro.macros.{AsGenericContainer, AsIndexedRecord}

class Test extends FlatSpec with Matchers {


  @AsGenericContainer("""{"type":"record","name":"MyClass","fields":[{"name":"a","type":"int"},{"name":"s","type":"string"},{"name":"f","type":"float"},{"name":"bts","type":"bytes"}]}""")
  case class MyClass(a: Int, s: String, f: Float, bts: Array[Byte])

  @AsIndexedRecord("""{"type":"record","name":"MyClass","fields":[{"name":"a","type":"int"},{"name":"s","type":"string"},{"name":"f","type":"float"},{"name":"bts","type":"bytes"}]}""")
  case class MyClass2(var a: Int, var s: String, var f: Float, var bts: Array[Byte])

  "AsGeneric" should "add getSchema method" in {
    x(MyClass(23, "asd", 3, Array(10)))
    x(MyClass2(23, "asd", 3, Array(10)))
    y(MyClass2(23, "asd", 3,  Array(10)))
  }

  def x(g: GenericContainer): Unit = {
    println(g.getSchema)
  }
  def y(ir: IndexedRecord): Unit = {
    println(ir.getSchema)
    println(ir.get(2))
    ir.put(2, 7f)
    println(ir.get(2))
    ir.put(3, Array[Byte](1,2,3))
    println(ir.get(3).asInstanceOf[Array[Byte]].mkString(","))
  }
}
