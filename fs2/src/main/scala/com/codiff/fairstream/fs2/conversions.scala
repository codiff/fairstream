package com.codiff.fairstream
package fs2

import _root_.fs2.{Pull, Pure, Stream}

object conversions {

  def fairToStream[A](fair: Fair[A]): Stream[Pure, A] = {
    def go(f: Fair[A]): Pull[Pure, A, Unit] = f match {
      case Fair.Nil                         => Pull.done
      case Fair.One(a)                      => Pull.output1(a)
      case c: Fair.Choice[A @unchecked]     => Pull.output1(c.a) >> go(c.rest)
      case i: Fair.Incomplete[A @unchecked] => go(i.step)
    }
    go(fair).stream
  }

  def fairTToStream[F[_], A](fairT: FairT[F, A]): Stream[F, A] = {
    def go(ft: FairT[F, A]): Pull[F, A, Unit] =
      Pull.eval(ft.run).flatMap {
        case FairE.Nil()  => Pull.done
        case FairE.One(a) => Pull.output1(a)
        case c: FairE.Choice[F, A] @unchecked =>
          Pull.output1(c.a) >> go(c.rest)
        case inc: FairE.Incomplete[F, A] @unchecked =>
          go(inc.rest)
      }
    go(fairT).stream
  }

}
