package scalavro.codec

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import shapeless.{Generic, HList}


trait Codecs {
  def apply[A](a: A)(implicit codec: Codec[A]): Array[Byte] = codec.encode(a).getOrElse(???).toByteArray

  object implicits {
    implicit val int: Codec[Int] = intCodec
    implicit val long: Codec[Long] = longCodec
    implicit val str: Codec[String] = strCodec
    implicit val bool: Codec[Boolean] = boolCodec
    implicit val byes: Codec[Array[Byte]] = byteCodec
  }
  class CCHelper[CC] {
    def make[R <: HList](implicit gen: Generic.Aux[CC, R], codec: Codec[R]): Codec[CC] = ccCodec[CC, R]
  }
  implicit def ccCodec[CC, R <: HList](implicit gen: Generic.Aux[CC, R], codec: Codec[R]): Codec[CC] = codec.xmap(gen.from, gen.to)

  implicit val boolCodec: Codec[Boolean] = new Codec[Boolean] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Boolean]] = {
      val bvv = bits.toByteVector
      Attempt.successful(DecodeResult((bvv.head & 0xff) == 1, bvv.tail.toBitVector))
    }

    override def encode(value: Boolean): Attempt[BitVector] = Attempt.successful {
      if (value) BitVector(Array[Byte](1)) else BitVector(Array[Byte](0) )
    }
    override def sizeBound: SizeBound = SizeBound.exact(1)
  }

  implicit val intCodec: Codec[Int] = new Codec[Int] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Int]] = {
      def innerDecode(bv: ByteVector, n: Int, count: Int): (Int, BitVector) = {
        val b = bv.head & 0xff
        val nn = n ^ ((b & 0x7f) << (count * 7))
        if(b > 0x7f) {
          innerDecode(bv.tail, nn, count + 1)
        } else {
          (nn, bv.tail.toBitVector)
        }
      }
      val (n, remaining) = innerDecode(bits.toByteVector, 0, 0)
      val result = (n >>> 1) ^ -(n & 1)
      Attempt.successful(DecodeResult(result, remaining))
    }

    override def encode(n: Int): Attempt[BitVector] = {
      def innerEncode(bv: ByteVector, n: Int, pos: Int): ByteVector = {
        if ((n & ~0x7F) != 0) {
          innerEncode(bv :+ ((n | 0x80) & 0xFF).toByte, n >>> 7, pos + 1)
        } else {
          bv :+ n.toByte
        }
      }
      Attempt.successful{
        innerEncode(ByteVector.empty, (n << 1) ^ (n >> 31), 0).toBitVector
      }
    }

    override def sizeBound: SizeBound = SizeBound.bounded(1, 5)
  }

  implicit val longCodec: Codec[Long] = new Codec[Long] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = {
      def innerDecode(bv: ByteVector, n: Long, count: Int): (Long, BitVector) = {
        val b = bv.head & 0xff
        val nn = n ^ ((b & 0x7fL) << (count * 7))
        if(b > 0x7f) {
          innerDecode(bv.tail, nn, count + 1)
        } else {
          (nn, bv.tail.toBitVector)
        }
      }
      val (n, remaining) = innerDecode(bits.toByteVector, 0, 0)
      val result = (n >>> 1) ^ -(n & 1)
      Attempt.successful(DecodeResult(result, remaining))
    }

    override def encode(n: Long): Attempt[BitVector] = {
      def innerEncode(bv: ByteVector, n: Long, pos: Int): ByteVector = {
        if ((n & ~0x7F) != 0) {
          innerEncode(bv :+ ((n | 0x80) & 0xFF).toByte, n >>> 7, pos + 1)
        } else {
          bv :+ n.toByte
        }
      }
      Attempt.successful{
        innerEncode(ByteVector.empty, (n << 1) ^ (n >> 63), 0).toBitVector
      }
    }

    override def sizeBound: SizeBound = SizeBound.bounded(1, 10)
  }

  implicit val byteCodec: Codec[Array[Byte]] = new Codec[Array[Byte]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Array[Byte]]] = for {
      lengthResult <- intCodec.decode(bits)
      result <- fixedCodec(lengthResult.value).decode(lengthResult.remainder)
    } yield result

    override def encode(value: Array[Byte]): Attempt[BitVector] = for {
      bv <- intCodec.encode(value.length)
      bv2 <- fixedCodec(value.length).encode(value)
    } yield bv ++ bv2

    override def sizeBound: SizeBound = SizeBound.unknown
  }

  def fixedCodec(length: Int): Codec[Array[Byte]] = new Codec[Array[Byte]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Array[Byte]]] = {
      val (result, remain) = bits.toByteVector.splitAt(length.toLong)
      Attempt.successful(DecodeResult(result.toArray, remain.toBitVector))
    }

    override def encode(value: Array[Byte]): Attempt[BitVector] = {
      Attempt.successful(BitVector(value))
    }

    override def sizeBound: SizeBound = SizeBound.unknown
  }

  implicit val strCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): Attempt[BitVector] = for {
      bv <- scodec.codecs.utf8.encode(value)
      bv2 <- byteCodec.encode(bv.toByteArray)
    } yield bv2

    override def sizeBound: SizeBound = SizeBound.unknown

    override def decode(bits: BitVector): Attempt[DecodeResult[String]] = for {
      bv <- byteCodec.decode(bits)
      str <- scodec.codecs.utf8.decode(BitVector(bv.value))
      result = str.copy(remainder = bv.remainder)
    } yield result
  }
}