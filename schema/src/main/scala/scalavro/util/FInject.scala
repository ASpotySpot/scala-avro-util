package scalavro.util

import cats.Monad
import cats.syntax.flatMap._

trait FInject[F[_], A, B] {self =>
  def to(a: A): B
  def from(b: B): F[A]

  def compose[C](fInj: FInject[F, B, C])(implicit F: Monad[F]): FInject[F, A, C] = FInject.from(
    a => fInj.to(self.to(a)),
    c => fInj.from(c).flatMap(self.from)
  )
}

object FInject {
  def from[F[_], A, B](f: A => B, g: B => F[A]): FInject[F, A, B] = new FInject[F, A, B] {
    override def to(a: A): B = f(a)
    override def from(b: B): F[A] = g(b)
  }
}