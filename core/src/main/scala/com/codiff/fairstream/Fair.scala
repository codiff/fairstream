package com.codiff.fairstream

import cats.{Alternative, Monad}

sealed trait Fair[+A]

object Fair {
  case object Nil extends Fair[Nothing]
  case class One[+A](a: A) extends Fair[A]
  class Choice[+A](val a: A, expr: => Fair[A]) extends Fair[A] {
    lazy val rest: Fair[A] = expr
  }

  object Choice {
    def apply[A](a: A, expr: => Fair[A]): Choice[A] = new Choice(a, expr)

    def unapply[A](s: Choice[A]): Some[(A, Fair[A])] = Some((s.a, s.rest))
  }

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
    case Nil                     => Incomplete(right)
    case One(a)                  => Choice(a, right)
    case c: Choice[A @unchecked] => Choice(c.a, mplus(right, c.rest))
    case inc: Incomplete[A @unchecked] =>
      right match {
        case Nil           => inc
        case One(b)        => Choice(b, inc.step)
        case Choice(b, r2) => Choice(b, mplus(inc.step, r2))
        case Incomplete(j) => Incomplete(mplus(inc.step, j))
      }
  }

  @annotation.tailrec
  def runM[A](
      maxDepth: Option[Int],
      maxResults: Option[Int],
      stream: Fair[A],
      acc: List[A] = List.empty
  ): List[A] = {
    if (maxResults.exists(_ <= 0)) acc.reverse
    else
      stream match {
        case Nil    => acc.reverse
        case One(a) => (a :: acc).reverse
        case Choice(a, r) =>
          runM(maxDepth, maxResults.map(_ - 1), r, a :: acc)
        case Incomplete(i) =>
          if (maxDepth.exists(_ <= 0)) acc.reverse
          else runM(maxDepth.map(_ - 1), maxResults, i, acc)
      }
  }

  implicit val fairMonad: Monad[Fair] with Alternative[Fair] =
    new Monad[Fair] with Alternative[Fair] {
      def empty[A]: Fair[A] = Fair.empty

      def pure[A](a: A): Fair[A] = Fair.unit(a)

      def flatMap[A, B](fa: Fair[A])(f: A => Fair[B]): Fair[B] = fa match {
        case Nil    => Nil
        case One(a) => f(a)
        case c: Choice[A @unchecked] =>
          combineK(f(c.a), Incomplete(flatMap(c.rest)(f)))
        case i: Incomplete[A @unchecked] =>
          Incomplete(flatMap(i.step)(f))
      }

      def tailRecM[A, B](a: A)(f: A => Fair[Either[A, B]]): Fair[B] =
        flatMap(f(a)) {
          case Right(b)    => pure(b)
          case Left(nextA) => Incomplete(tailRecM(nextA)(f))
        }

      def combineK[A](x: Fair[A], y: Fair[A]): Fair[A] = mplus(x, y)
    }

}
