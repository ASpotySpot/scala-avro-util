package scalavro.builder

import cats.data.NonEmptyList
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString

object RefineUtils {
  implicit def nestr(s: String): NonEmptyString = refineV[NonEmpty](s).right.get
  implicit def nels[A](ls: List[A]): NonEmptyList[A] = NonEmptyList(ls.head, ls.tail)
}
