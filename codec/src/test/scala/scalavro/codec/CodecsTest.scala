package scalavro.codec

import java.io.ByteArrayOutputStream

import org.apache.avro.io.{BinaryEncoder, EncoderFactory}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scodec.Codec
import scodec.bits.BitVector

object CodecsTest extends Properties("AvroCodecs") {
  trait TestCase[A] {
    def javaEncode(javaEnc: BinaryEncoder, a: A): Unit
    def eq: (A, A) => Boolean = (a1, a2) => a1 == a2
    def codec(a: A): Codec[A]
    def testOne(a: A): Boolean = {
      def c: Codec[A] = codec(a)
      val baos = new ByteArrayOutputStream()
      val enc = EncoderFactory.get().binaryEncoder(baos, null)
      javaEncode(enc, a)
      enc.flush()
      baos.flush()
      val expected = baos.toByteArray.mkString(",")
      val result = c.encode(a).getOrElse(???)
      val encodeResult = result.toByteArray.mkString(",") == expected
      val decodeResult = eq(c.decode(result).getOrElse(???).value, a)
      decodeResult && encodeResult
    }
  }
  abstract class StaticCodecTestCase[A](staticCodec: Codec[A]) extends TestCase[A] {
    override def codec(a: A): Codec[A] = staticCodec
    def testList(as: List[A]): Boolean = {
      val baos = new ByteArrayOutputStream()
      val enc = EncoderFactory.get().binaryEncoder(baos, null)
      as.foreach { a =>
        javaEncode(enc, a)
      }
      enc.flush()
      baos.flush()
      val expectedEncode = baos.toByteArray
      val encodeResult = as.foldLeft(BitVector.empty){(bv, a) =>
        bv ++ codec(a).encode(a).getOrElse(???)
      }
      val encodeBool = encodeResult.toByteArray.mkString(",") == expectedEncode.mkString(",")
      def doDecode(bv: BitVector): List[A] = {
        if(bv.isEmpty) {
          Nil
        } else {
          val result = staticCodec.decode(bv).require
          result.value :: doDecode(result.remainder)
        }
      }
      val decodeResult = doDecode(BitVector(expectedEncode))
      val decodeBool = decodeResult.length == as.length && decodeResult.zip(as).forall{case (a1, a2) => eq(a1, a2)}
      encodeBool && decodeBool
    }
  }
  abstract class ImplicitTestCase[A: Codec] extends StaticCodecTestCase[A](implicitly)

  object BoolTest extends ImplicitTestCase[Boolean] {
    override def javaEncode(javaEnc: BinaryEncoder, a: Boolean): Unit = javaEnc.writeBoolean(a)
  }
  object IntTest extends ImplicitTestCase[Int] {
    override def javaEncode(javaEnc: BinaryEncoder, a: Int): Unit = javaEnc.writeInt(a)
  }
  object LongTest extends ImplicitTestCase[Long] {
    override def javaEncode(javaEnc: BinaryEncoder, a: Long): Unit = javaEnc.writeLong(a)
  }
  object FixedTest extends TestCase[Array[Byte]] {
    override def javaEncode(javaEnc: BinaryEncoder, a: Array[Byte]): Unit = javaEnc.writeFixed(a)
    override def eq: (Array[Byte], Array[Byte]) => Boolean = (b1, b2) => b1.mkString(",") == b2.mkString(",")
    override def codec(a: Array[Byte]): Codec[Array[Byte]] = scalavro.codec.fixedCodec(a.length)
  }
  object BytesTest extends ImplicitTestCase[Array[Byte]] {
    override def javaEncode(javaEnc: BinaryEncoder, a: Array[Byte]): Unit = javaEnc.writeBytes(a)
    override def eq: (Array[Byte], Array[Byte]) => Boolean = (b1, b2) => b1.mkString(",") == b2.mkString(",")
  }
  object StringTest extends ImplicitTestCase[String] {
    override def javaEncode(javaEnc: BinaryEncoder, a: String): Unit = javaEnc.writeString(a)
  }

  //  property("ManualTest") = forAll{i: List[String] => StringTest.testList(List("", ""))}
  //
  property("Bool") = forAll{i: Boolean => BoolTest.testOne(i)}
  property("Int") = forAll{i: Int => IntTest.testOne(i)}
  property("Long") = forAll{i: Long => LongTest.testOne(i)}
  property("Fixed") = forAll{i: List[Byte] => FixedTest.testOne(i.toArray)}
  property("Bytes") = forAll{i: List[Byte] => BytesTest.testOne(i.toArray)}
  property("String") = forAll{i: String => StringTest.testOne(i)}

  property("Bool.List") = forAll{i: List[Boolean] => BoolTest.testList(i)}
  property("Int.List") = forAll{i: List[Int] => IntTest.testList(i)}
  property("Long.List") = forAll{i: List[Long] => LongTest.testList(i)}

  property("Bytes.List") = forAll{i: List[List[Byte]] => BytesTest.testList(i.map(_.toArray))}
  property("String.List") = forAll{i: List[String] => StringTest.testList(i)}

}
