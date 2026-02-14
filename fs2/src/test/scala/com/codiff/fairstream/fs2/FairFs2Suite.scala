package com.codiff.fairstream
package fs2

import scala.concurrent.duration._

import cats.effect.IO
import cats.syntax.all._
import munit.CatsEffectSuite

import syntax._

class FairFs2Suite extends CatsEffectSuite {

  test("Fair.toFs2: empty stream") {
    val result = Fair.empty[Int].toFs2.toList
    assertEquals(result, List.empty[Int])
  }

  test("Fair.toFs2: single element") {
    val result = Fair.unit(42).toFs2.toList
    assertEquals(result, List(42))
  }

  test("Fair.toFs2: finite stream") {
    import Fair._
    val stream = mplus(unit(1), mplus(unit(2), unit(3)))
    val result = stream.toFs2.toList
    val expected = Fair.runM(None, None, stream)
    assertEquals(result, expected)
  }

  test("Fair.toFs2: infinite stream (take N)") {
    import Fair._
    lazy val number: Fair[Int] = mplus(unit(0), number.map(_ + 1))
    val result = number.toFs2.take(20).toList
    val expected = Fair.runM(None, Some(20), number)
    assertEquals(result, expected)
  }

  test("Fair.toFs2: pythagorean triples match runM output") {
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

    val n = 7
    val result = triples.toFs2.take(n.toLong).toList
    val expected = Fair.runM(None, Some(n), triples)
    assertEquals(result, expected)
  }

  test("FairT[IO].toFs2: empty stream") {
    FairT.empty[IO, Int].toFs2.compile.toList.map { result =>
      assertEquals(result, List.empty[Int])
    }
  }

  test("FairT[IO].toFs2: single element") {
    FairT.unit[IO, Int](42).toFs2.compile.toList.map { result =>
      assertEquals(result, List(42))
    }
  }

  test("FairT[IO].toFs2: finite stream") {
    val stream = FairT.mplus[IO, Int](
      FairT.unit[IO, Int](1),
      FairT.mplus[IO, Int](
        FairT.unit[IO, Int](2),
        FairT.unit[IO, Int](3)
      )
    )
    for {
      result <- stream.toFs2.compile.toList
      expected <- FairT.runM[IO, Int](None, None, stream)
    } yield assertEquals(result, expected)
  }

  test("FairT[IO].toFs2: pythagorean triples match runM output") {
    def guardF(cond: Boolean): FairT[IO, Unit] =
      if (cond) FairT.unit[IO, Unit](()) else FairT.empty[IO, Unit]

    lazy val number: FairT[IO, Int] =
      FairT.mplus[IO, Int](FairT.unit[IO, Int](0), number.map(_ + 1))

    val triples: FairT[IO, (Int, Int, Int)] = for {
      i <- number
      _ <- guardF(i > 0)
      j <- number
      _ <- guardF(j > 0)
      k <- number
      _ <- guardF(k > 0)
      _ <- guardF(i * i + j * j == k * k)
    } yield (i, j, k)

    val n = 7
    for {
      result <- triples.toFs2.take(n.toLong).compile.toList
      expected <- FairT.runM[IO, (Int, Int, Int)](None, Some(n), triples)
    } yield assertEquals(result, expected)
  }

  // -- Plain fs2.Stream vs Fair interleaving --

  test(
    "plain fs2.Stream cannot find pythagorean triples (depth-first gets stuck)"
  ) {
    // Plain fs2.Stream uses depth-first (sequential) flatMap: for i=1, j=1 it
    // tries k=1,2,3,... forever, never advancing j or i. So it cannot produce
    // even a single triple from an infinite number stream within a budget.
    val number: _root_.fs2.Stream[IO, Int] = _root_.fs2.Stream.iterate(1)(_ + 1)

    val triples = for {
      i <- number
      j <- number
      k <- number
      if i * i + j * j == k * k
    } yield (i, j, k)

    // Give it a generous timeout — still finds nothing.
    triples
      .take(1)
      .interruptAfter(3.seconds)
      .compile
      .toList
      .map { result =>
        assertEquals(result, List.empty[(Int, Int, Int)])
      }
  }

  test("Fair.toFs2 finds pythagorean triples thanks to fair interleaving") {
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

    val result = triples.toFs2.take(7).toList
    assertEquals(result.length, 7)
    assert(result.forall { case (i, j, k) => i * i + j * j == k * k })
  }

}
