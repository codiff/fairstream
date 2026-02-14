package com.codiff.fairstream
package fs2

import _root_.fs2.{Pure, Stream}

object syntax {

  implicit class FairFs2Ops[A](val fair: Fair[A]) extends AnyVal {
    def toFs2: Stream[Pure, A] = conversions.fairToStream(fair)
  }

  implicit class FairTFs2Ops[F[_], A](val fairT: FairT[F, A]) extends AnyVal {
    def toFs2: Stream[F, A] =
      conversions.fairTToStream(fairT)
  }

}
