package scalavro.schema.types

sealed trait AvscOrder

object AvscOrder {
  case object Ascending extends AvscOrder
  case object Descending extends AvscOrder
}