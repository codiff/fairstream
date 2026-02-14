/*
 * Copyright 2026 codiff
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.codiff.fairstream

import cats.{Alternative, Applicative, Monad, ~>}

sealed trait FairE[M[_], A]

object FairE {
  final case class Nil[M[_], A]() extends FairE[M, A]
  final case class One[M[_], A](a: A) extends FairE[M, A]
  class Choice[M[_], A](val a: A, expr: => FairT[M, A]) extends FairE[M, A] {
    lazy val rest: FairT[M, A] = expr
  }

  object Choice {
    def apply[M[_], A](a: A, expr: => FairT[M, A]): Choice[M, A] =
      new Choice(a, expr)

    def unapply[M[_], A](s: Choice[M, A]): Some[(A, FairT[M, A])] = Some(
      (s.a, s.rest)
    )
  }
  class Incomplete[M[_], A](expr: => FairT[M, A]) extends FairE[M, A] {
    lazy val rest: FairT[M, A] = expr
  }

  object Incomplete {
    def apply[M[_], A](expr: => FairT[M, A]): Incomplete[M, A] = new Incomplete(
      expr
    )

    def unapply[M[_], A](s: Incomplete[M, A]): Some[FairT[M, A]] = Some(s.rest)
  }
}

final case class FairT[M[_], A](run: M[FairE[M, A]])

object FairT {
  def empty[M[_], A](implicit M: Applicative[M]): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.Nil()))

  def unit[M[_], A](a: A)(implicit M: Applicative[M]): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.One(a)))

  def suspend[M[_], A](s: => FairT[M, A])(implicit
      M: Applicative[M]
  ): FairT[M, A] =
    FairT(M.pure[FairE[M, A]](FairE.Incomplete(s)))

  def lift[M[_], A](ma: M[A])(implicit M: Monad[M]): FairT[M, A] =
    FairT(M.map(ma)(a => FairE.One[M, A](a): FairE[M, A]))

  def liftK[M[_]](implicit M: Monad[M]): M ~> FairT[M, *] =
    new (M ~> FairT[M, *]) {
      def apply[A](ma: M[A]): FairT[M, A] = lift(ma)
    }

  def mplus[M[_], A](left: FairT[M, A], right: => FairT[M, A])(implicit
      M: Monad[M]
  ): FairT[M, A] = {
    type E = FairE[M, A]
    FairT(M.flatMap[E, E](left.run) {
      case FairE.Nil()  => M.pure[E](FairE.Incomplete(right))
      case FairE.One(a) => M.pure[E](FairE.Choice(a, right))
      case c: FairE.Choice[M, A] @unchecked =>
        M.pure[E](FairE.Choice(c.a, mplus(right, c.rest)))
      case inc: FairE.Incomplete[M, A] @unchecked =>
        M.flatMap[E, E](right.run) {
          case FairE.Nil()  => M.pure[E](inc)
          case FairE.One(b) => M.pure[E](FairE.Choice(b, inc.rest))
          case rc: FairE.Choice[M, A] @unchecked =>
            M.pure[E](FairE.Choice(rc.a, mplus(inc.rest, rc.rest)))
          case rinc: FairE.Incomplete[M, A] @unchecked =>
            M.pure[E](FairE.Incomplete(mplus(inc.rest, rinc.rest)))
        }
    })
  }

  def flatMap[M[_], A, B](
      fa: FairT[M, A]
  )(f: A => FairT[M, B])(implicit M: Monad[M]): FairT[M, B] = {
    type EB = FairE[M, B]
    FairT(M.flatMap[FairE[M, A], EB](fa.run) {
      case FairE.Nil()  => M.pure[EB](FairE.Nil())
      case FairE.One(a) => f(a).run
      case c: FairE.Choice[M, A] @unchecked =>
        mplus(f(c.a), suspend(flatMap(c.rest)(f))).run
      case i: FairE.Incomplete[M, A] @unchecked =>
        M.pure[EB](FairE.Incomplete(flatMap(i.rest)(f)))
    })
  }

  def runM[M[_], A](
      maxDepth: Option[Int],
      maxResults: Option[Int],
      stream: FairT[M, A]
  )(implicit M: Monad[M]): M[List[A]] = {
    if (maxResults.exists(_ <= 0)) M.pure(List.empty)
    else
      M.flatMap(stream.run) {
        case FairE.Nil()  => M.pure(List.empty)
        case FairE.One(a) => M.pure(List(a))
        case c: FairE.Choice[M, A] @unchecked =>
          M.map(runM(maxDepth, maxResults.map(_ - 1), c.rest))(c.a :: _)
        case inc: FairE.Incomplete[M, A] @unchecked =>
          if (maxDepth.exists(_ <= 0)) M.pure(List.empty)
          else runM(maxDepth.map(_ - 1), maxResults, inc.rest)
      }
  }

  implicit def fairTMonad[M[_]: Monad]
      : Monad[FairT[M, *]] with Alternative[FairT[M, *]] =
    new Monad[FairT[M, *]] with Alternative[FairT[M, *]] {
      def empty[A]: FairT[M, A] = FairT.empty

      def pure[A](a: A): FairT[M, A] = FairT.unit(a)

      def flatMap[A, B](fa: FairT[M, A])(f: A => FairT[M, B]): FairT[M, B] =
        FairT.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => FairT[M, Either[A, B]]): FairT[M, B] = {
        val MM = Monad[M]
        type E = FairE[M, B]
        val cont: Either[A, B] => FairT[M, B] = {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => FairT.unit(b)
        }
        FairT[M, B](MM.tailRecM[A, E](a) { a =>
          MM.map[FairE[M, Either[A, B]], Either[A, E]](f(a).run) {
            case FairE.Nil()         => Right(FairE.Nil())
            case FairE.One(Left(a))  => Left(a)
            case FairE.One(Right(b)) => Right(FairE.One(b))
            case c: FairE.Choice[M, Either[A, B]] @unchecked =>
              val rest: FairT[M, B] =
                FairT.flatMap[M, Either[A, B], B](c.rest)(cont)
              c.a match {
                case Right(b) => Right(FairE.Choice(b, rest))
                case Left(a) =>
                  Right(FairE.Incomplete(mplus[M, B](tailRecM(a)(f), rest)))
              }
            case inc: FairE.Incomplete[M, Either[A, B]] @unchecked =>
              Right(
                FairE.Incomplete(
                  FairT.flatMap[M, Either[A, B], B](inc.rest)(cont)
                )
              )
          }
        })
      }

      def combineK[A](x: FairT[M, A], y: FairT[M, A]): FairT[M, A] = mplus(x, y)
    }

}
