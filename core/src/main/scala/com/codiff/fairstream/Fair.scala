package com.codiff.fairstream

import cats.{Alternative, Monad, StackSafeMonad}

sealed trait Fair[+A]

object Fair {
  case object Nil extends Fair[Nothing]
  case class One[+A](a: A) extends Fair[A]
  case class Choice[+A](a: A, rest: Fair[A]) extends Fair[A]

  class Incomplete[+A](expr: => Fair[A]) extends Fair[A] {
    lazy val step: Fair[A] = expr
  }

  object Incomplete {
    def apply[A](expr: => Fair[A]): Incomplete[A] = new Incomplete(expr)

    def unapply[A](s: Incomplete[A]): Some[Fair[A]] = Some(s.step)
  }

  def empty[A]: Fair[A] = Nil

  def unit[A](a: A): Fair[A] = One(a)

  def constant[A](a: A): Fair[A] = Choice(a, Incomplete(constant(a)))

  def guard(cond: Boolean): Fair[Unit] = if (cond) unit(()) else empty

  def mplus[A](left: Fair[A], right: => Fair[A]): Fair[A] = left match {
    case Nil          => Incomplete(right)
    case One(a)       => Choice(a, right)
    case Choice(a, r) => Choice(a, mplus(right, r))
    case Incomplete(i) =>
      right match {
        case Nil           => Incomplete(i)
        case One(b)        => Choice(b, i)
        case Choice(b, r2) => Choice(b, Incomplete(mplus(i, r2)))
        case Incomplete(j) => Incomplete(mplus(i, j))
      }
  }

  implicit val fairMonad
      : Monad[Fair] with Alternative[Fair] with StackSafeMonad[Fair] =
    new Monad[Fair] with Alternative[Fair] with StackSafeMonad[Fair] {
      def empty[A]: Fair[A] = Fair.empty

      def pure[A](a: A): Fair[A] = Fair.unit(a)

      def flatMap[A, B](fa: Fair[A])(f: A => Fair[B]): Fair[B] = fa match {
        case Nil           => Nil
        case One(a)        => f(a)
        case Choice(a, r)  => combineK(f(a), Incomplete(flatMap(r)(f)))
        case Incomplete(i) => Incomplete(flatMap(i)(f))
      }

      def combineK[A](x: Fair[A], y: Fair[A]): Fair[A] = mplus(x, y)
    }

}
