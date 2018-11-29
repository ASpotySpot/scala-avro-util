package scalavro.schema.builder

import cats.data.NonEmptyList

case class StuffContext[A](a: A, stuff: StuffToBuild) {
  def addStuff(newStuff: StuffToBuild): StuffContext[A] = new StuffContext[A](a, newStuff |+| stuff)
  def map[B](f: A => B): StuffContext[B] = new StuffContext[B](f(a), stuff)
  def flatMap[B](f: A => StuffContext[B]): StuffContext[B] = f(a).addStuff(stuff)
}

private object StuffContext {
  def empty[A](a: A): StuffContext[A] = new StuffContext[A](a, StuffToBuild.empty)

  def sequence[A](ls: NonEmptyList[StuffContext[A]]): StuffContext[List[A]] = {
    ls.foldLeft(StuffContext.empty(List.empty[A])) { (ctxAs, ctxA) =>
      for {
        as <- ctxAs
        a <- ctxA
      } yield a :: as
    }
  }
}
