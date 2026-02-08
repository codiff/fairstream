package com.codiff.fairstream

import cats.{Alternative, Applicative, Monad}

sealed trait FairE[M[_], A]

object FairE {
  final case class Nil[M[_], A]() extends FairE[M, A]
  final case class One[M[_], A](a: A) extends FairE[M, A]
  final case class Choice[M[_], A](a: A, rest: FairT[M, A]) extends FairE[M, A]
  final case class Incomplete[M[_], A](rest: FairT[M, A]) extends FairE[M, A]
}

final case class FairT[M[_], A](run: M[FairE[M, A]])

object FairT {
  def empty[M[_], A](implicit M: Applicative[M]): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.Nil()))

  def unit[M[_], A](a: A)(implicit M: Applicative[M]): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.One(a)))

  def suspend[M[_], A](s: FairT[M, A])(implicit
      M: Applicative[M]
  ): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.Incomplete(s)))

  def mplus[M[_], A](left: FairT[M, A], right: => FairT[M, A])(implicit
      M: Monad[M]
  ): FairT[M, A] = {
    type E = FairE[M, A]
    FairT(M.flatMap[E, E](left.run) {
      case FairE.Nil()        => M.pure[E](FairE.Incomplete(right))
      case FairE.One(a)       => M.pure[E](FairE.Choice(a, right))
      case FairE.Choice(a, r) => M.pure[E](FairE.Choice(a, mplus(right, r)))
      case FairE.Incomplete(i) =>
        M.map[E, E](right.run) {
          case FairE.Nil()         => FairE.Incomplete(i)
          case FairE.One(b)        => FairE.Choice(b, i)
          case FairE.Choice(b, r2) => FairE.Choice(b, mplus(i, r2))
          case FairE.Incomplete(j) => FairE.Incomplete(mplus(i, j))
        }
    })
  }

  def flatMap[M[_], A, B](
      fa: FairT[M, A]
  )(f: A => FairT[M, B])(implicit M: Monad[M]): FairT[M, B] = {
    type EB = FairE[M, B]
    FairT(M.flatMap[FairE[M, A], EB](fa.run) {
      case FairE.Nil()         => M.pure[EB](FairE.Nil())
      case FairE.One(a)        => f(a).run
      case FairE.Choice(a, r)  => mplus(f(a), suspend(flatMap(r)(f))).run
      case FairE.Incomplete(i) => M.pure[EB](FairE.Incomplete(flatMap(i)(f)))
    })
  }

  implicit def fairTMonad[M[_]: Monad]
      : Monad[FairT[M, *]] with Alternative[FairT[M, *]] =
    new Monad[FairT[M, *]] with Alternative[FairT[M, *]] {
      def empty[A]: FairT[M, A] = FairT.empty

      def pure[A](a: A): FairT[M, A] = FairT.unit(a)

      def flatMap[A, B](fa: FairT[M, A])(f: A => FairT[M, B]): FairT[M, B] =
        FairT.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => FairT[M, Either[A, B]]): FairT[M, B] =
        flatMap(f(a)) {
          case Left(next) => tailRecM(next)(f)
          case Right(b)   => FairT.unit(b)
        }

      def combineK[A](x: FairT[M, A], y: FairT[M, A]): FairT[M, A] = mplus(x, y)
    }

}
