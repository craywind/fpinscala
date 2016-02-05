import fpinscala.errorhandling
import fpinscala.errorhandling._

val a: Option[Int] = Some(3)
val b: Option[Int] = None
val x: Exception = new IndexOutOfBoundsException

a.filter(_ > 2)
a filter(_ % 2 == 0)
b filter((_) => true)
a.flatMap((x) => Some(x + 3))
b.flatMap((x) => Some(x + 3))
a.flatMap((x) => None)
a.getOrElse(2)
b.getOrElse(2)
a.map(_ + 2)
b.map(_ + 2)
a.orElse(Some(4))
b.orElse(Some(4))
Option.variance(Seq(3.0, 4.0, 7.0, 10.0))
Option.variance(Seq())
Option.sequence(List(Some(2), Some(5), Some(0)))
Option.sequence(List(Some(2), Some(5), Some(0), None))

val c: Either[Exception, Int] = Right(5)
val d: Either[Exception, Int] = Left(x)
c.map(_ + 2)
c.map((x) => 3 / (5 - x))
d.map(_ + 2)
c.flatMap((x) => Right(x + 2))
d.flatMap((x) => Right(x + 2))
c.map2(Right(3.0))(_ / _)
val e: List[Either[Exception, Int]] = List(Right(1), Right(2), Right(3), Left(x), Right(5))
val f: List[Either[Exception, Int]] = List(Right(1), Right(2), Right(3), Right(4), Right(5))
Either.sequence(e)
Either.sequence(f)
Either.traverse(List(2, 1, 0))((x) => Either.Try(2 / x))
Either.traverse(List(2, 1, 3))((x) => Either.Try(2 / x))







