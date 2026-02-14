## fairstream

[Simple fair and terminating backtracking Monad Transformer](https://okmij.org/ftp/Computation/monads.html#fair-bt-stream)

[Backtracking, Interleaving, and Terminating Monad Transformers](https://okmij.org/ftp/Computation/LogicT.pdf)

### Motivation

The problem with depth-first search `flatMap` of [https://fs2.io/](`fs2.Stream`) and standard library's collection is that when you nest infinite streams, it gets stuck exploring the first branch forever:

```scala
val number = fs2.Stream.iterate(1)(_ + 1) // 1, 2, 3, ...

val triples = for {
  i <- number
  j <- number // stuck here: tries j=1,2,3,... for i=1 forever
  k <- number
  if i * i + j * j == k * k
} yield (i, j, k)

triples.take(1).toList // never terminates
```

`Fair` uses fair disjunction (`mplus`) instead of simple concatenation. It interleaves branches so that every candidate is eventually reached:

```scala
import com.codiff.fairstream.Fair
import com.codiff.fairstream.Fair._

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

Fair.runM(None, Some(5), triples)
// List((3,4,5), (4,3,5), (6,8,10), (8,6,10), (5,12,13))
```

The `fairstream-fs2` allows converting `Fair`/`FairT` to `fs2.Stream`:

```scala
import com.codiff.fairstream.fs2.syntax._

// Fair[A] => fs2.Stream[Pure, A]
val stream = triples.toFs2.take(10).toList

// FairT[IO, A] => fs2.Stream[IO, A]
val ioStream = compatible.toFs2.take(5).compile.toList
```
