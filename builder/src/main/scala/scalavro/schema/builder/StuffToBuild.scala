package scalavro.schema.builder

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import scalavro.schema.Record

case class StuffToBuild(records: List[Record],
                        enums: List[(NonEmptyString, NonEmptyString, NonEmptyList[NonEmptyString])]) {
  def |+|(newStuff: StuffToBuild): StuffToBuild = StuffToBuild(records ++ newStuff.records, enums ++ newStuff.enums)
}

private object StuffToBuild {
  def empty: StuffToBuild = StuffToBuild(List.empty, List.empty)

  def flatten(stuffs: List[StuffToBuild]): StuffToBuild = stuffs.reduceOption(_ |+| _).getOrElse(StuffToBuild.empty)
}
