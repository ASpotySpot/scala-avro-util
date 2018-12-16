package scalavro.schema.parser

import cats.data.NonEmptyList
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import scalavro.schema.types.AvscType._
import scalavro.schema.types.ToAvscType
import shapeless.labelled.FieldType
import shapeless.ops.hlist.ToTraversable
import shapeless.{::, HList, LabelledGeneric, _}

import scala.reflect.ClassTag

trait Avroable[A] extends DepFn0 with Serializable {
  type Out <: HList
}



object Avroable {
  def force(s: String): NonEmptyString = refineV[NonEmpty](s).right.get
  def force[A](ls: List[A]): NonEmptyList[A] = NonEmptyList(ls.head, ls.tail)
  type Aux[L <: HList, Out0 <: HList] = Avroable[L] {type Out = Out0}

  def apply[L <: HList](implicit at: Avroable[L]): Aux[L, at.Out] = at

  implicit def hnilAvr[L <: HNil]: Aux[L, HNil] =
    new Avroable[L] {
      type Out = HNil

      def apply(): Out = HNil
    }

  implicit def hlistAvr[K <: Symbol, V, T <: HList](implicit
                                                     w: Witness.Aux[K],
                                                     ath: ToAvscType[V],
                                                     att: Avroable[T]): Aux[FieldType[K, V] :: T, Field :: att.Out] =
    new Avroable[FieldType[K, V] :: T] {
      type Out = Field :: att.Out

      def apply(): Out = Field(refineV[NonEmpty](w.value.name).right.get, None, ath.apply)(None) :: att()
    }

  implicit def genAvr[K <: Symbol,
                      V,
                      R <: HList,
                      O <: HList,
                      T <: HList](implicit
//                                  gen: LabelledGeneric.Aux[V, R],
                                  w: Witness.Aux[K],
                                  ct: ClassTag[V],
                                  avrH: Avroable.Aux[R, O],
                                  toTraversable: ToTraversable.Aux[O, List, Field],
                                  avrT: Lazy[Avroable[T]]): Aux[FieldType[K, V] :: T, Field :: avrT.value.Out] = {

    new Avroable[FieldType[K, V] :: T] {
      override type Out = Field :: avrT.value.Out

      override def apply(): Out = {
        val r: Record = Record(
          force(ct.runtimeClass.getSimpleName),
          Some(ct.runtimeClass.getPackage.getName).map(s => refineV[NonEmpty](s).right.get),
          None,
          None,
          force(avrH.apply().toList)
        )
        Field(force(w.value.name), None, r)(None) :: avrT.value()
      }
    }
  }

  implicit def avr[CC, R <: HList, O <: HList](implicit ct: ClassTag[CC],
                                               gen: LabelledGeneric.Aux[CC, R],
                                               toAvroType: Avroable.Aux[R, O],
                                               toTraversable: ToTraversable.Aux[O, List, Field]): Record = {
    Record(
      force(ct.runtimeClass.getSimpleName),
      Some(ct.runtimeClass.getPackage.getName).map(force),
      None,
      None,
      force(Avroable[gen.Repr].apply().toList)
    )
  }

  class Helper[CC](implicit ct: ClassTag[CC]) {
    def make[R <: HList, O <: HList](implicit gen: LabelledGeneric.Aux[CC, R],
                                     toAvroType: Avroable.Aux[R, O],
                                     toTraversable: ToTraversable.Aux[O, List, Field]): Record = {
      avr[CC, R, O]
    }
  }
}