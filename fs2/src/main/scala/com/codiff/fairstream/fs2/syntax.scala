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
