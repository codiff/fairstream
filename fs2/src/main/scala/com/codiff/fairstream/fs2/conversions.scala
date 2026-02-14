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
