package com.codiff.fairstream

import cats.Eval
import cats.syntax.all._
import munit.FunSuite

class PythagoreanSuite extends FunSuite {

  def isPythagorean(t: (Int, Int, Int)): Boolean = {
    val (i, j, k) = t
    i * i + j * j == k * k
  }

  // -- Fair tests (FBackTrack.hs) --

  test("Fair: pythagorean triples (generic MonadPlus style)") {
    import Fair._

    lazy val number: Fair[Int] = mplus(unit(0), number.map(_ + 1))

    val triples = for {
      i <- number
      _ <- guard(i > 0)
      j <- number
      _ <- guard(j > 0)
      k <- number
      _ <- guard(k > 0)
      _ <- guard(i * i + j * j == k * k)
    } yield (i, j, k)

    val results = Fair.runM(None, Some(7), triples)
    assertEquals(results.length, 7)
    assert(results.forall(isPythagorean))
  }

  test("Fair: pythagorean triples with left recursion") {
    import Fair._

    lazy val number: Fair[Int] =
      mplus((Incomplete(number): Fair[Int]).map(_ + 1), unit(0))

    val triples = for {
      i <- number
      j <- number
      k <- number
      _ <- guard(i * i + j * j == k * k)
    } yield (i, j, k)

    val results = Fair.runM(None, Some(27), triples)
    assertEquals(results.length, 27)
    assert(results.forall(isPythagorean))
  }

  // -- FairT tests (FBackTrackT.hs) --
  // Using Eval as M because:
  // - Id is strict with no trampolining → stack overflow on deep Incomplete chains
  // - Eval provides stack-safe trampolined flatMap with minimal overhead
  // - IO works but has too much per-step overhead for large searches

  def guardF(cond: Boolean): FairT[Eval, Unit] =
    if (cond) FairT.unit[Eval, Unit](()) else FairT.empty[Eval, Unit]

  test("FairT[Eval]: pythagorean triples (generic MonadPlus style)") {
    lazy val number: FairT[Eval, Int] =
      FairT.mplus[Eval, Int](FairT.unit[Eval, Int](0), number.map(_ + 1))

    val triples: FairT[Eval, (Int, Int, Int)] = for {
      i <- number
      _ <- guardF(i > 0)
      j <- number
      _ <- guardF(j > 0)
      k <- number
      _ <- guardF(k > 0)
      _ <- guardF(i * i + j * j == k * k)
    } yield (i, j, k)

    val results =
      FairT.runM[Eval, (Int, Int, Int)](None, Some(7), triples).value
    assertEquals(results.length, 7)
    assert(results.forall(isPythagorean))
  }

  test("FairT[Eval]: pythagorean triples with left recursion") {
    lazy val number: FairT[Eval, Int] =
      FairT.mplus[Eval, Int](
        FairT.suspend[Eval, Int](number).map(_ + 1),
        FairT.unit[Eval, Int](0)
      )

    val triples: FairT[Eval, (Int, Int, Int)] = for {
      i <- number
      j <- number
      k <- number
      _ <- guardF(i * i + j * j == k * k)
    } yield (i, j, k)

    val results =
      FairT.runM[Eval, (Int, Int, Int)](None, Some(27), triples).value
    assertEquals(results.length, 27)
    assert(results.forall(isPythagorean))
  }
}
