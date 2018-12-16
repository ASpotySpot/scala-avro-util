package scalavro.builder

import scalavro.schema.types.AvscType._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless._
import scalavro.schema.types.AvscType


object ArbAvsc {
  implicit val arbString: Arbitrary[NonEmptyString] = Arbitrary {
    Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString).map(s => refineV[NonEmpty](s).right.get)
  }
  implicit val simpleAvscType: Gen[SimpleAvscType] = Gen.oneOf(
    NullType,
    BoolType,
    IntType,
    LongType,
    FloatType,
    DoubleType,
    StringType,
    BytesType
  )
  implicit val complexAvscType: Gen[ComplexAvscType] = {
    Gen.oneOf(
      Arbitrary.arbitrary[EnumType],
      Arbitrary.arbitrary[Fixed],
      Arbitrary.arbitrary[Record],
      Arbitrary.arbitrary[MapType],
      Arbitrary.arbitrary[Union],
      simpleAvscType.map(t => ArrayType(t))
    )
  }
  implicit val avscType: Arbitrary[AvscType] = Arbitrary(Gen.oneOf(simpleAvscType, complexAvscType))
  implicit val field: Arbitrary[Field] = Arbitrary {
    for {
      name <- Arbitrary.arbitrary[NonEmptyString]
      doc <- Gen.option(Arbitrary.arbitrary[NonEmptyString]).map(_.map(_.value))
      at <- Arbitrary.arbitrary[AvscType]
    } yield Field(name, doc, at)(None)
  }
}
