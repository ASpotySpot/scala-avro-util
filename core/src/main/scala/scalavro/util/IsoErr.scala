package scalavro.util

import cats.Monad
import cats.syntax.flatMap._

trait IsoErr[F[_], A, B] {self =>
  def to(a: A): F[B]
  def from(b: B): F[A]

  def compose[C](isoErr: IsoErr[F, B, C])(implicit F: Monad[F]): IsoErr[F, A, C] = IsoErr.from(
    a => self.to(a).flatMap(isoErr.to),
    c => isoErr.from(c).flatMap(self.from)
  )
}

object IsoErr {
  def from[F[_], A, B](f: A => F[B], g: B => F[A]): IsoErr[F, A, B] = new IsoErr[F, A, B] {
    override def to(a: A): F[B] = f(a)
    override def from(b: B): F[A] = g(b)
  }
}