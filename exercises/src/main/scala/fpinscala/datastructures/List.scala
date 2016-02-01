package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("End of list reached")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("End of list reached")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => throw new IndexOutOfBoundsException("End of list reached")
      case Cons(_, t) => drop(t, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("Can not inin on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)


  def reverse[A](l : List[A]): List[A] = {
    @annotation.tailrec
    def reverseAcc[A](o: List[A], r: List[A]): List[A] = o match {
      case Nil => r
      case Cons(h, t) => reverseAcc(t, Cons(h, r))
    }
    reverseAcc(l, Nil)
  }

  def reverse2[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((r, h) => Cons(h, r))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y,x))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)(Cons(_,_))

  def flatten[A](l: List[List[A]]): List[A] = {
    def flattenAcc[A](o: List[List[A]], n: List[A]): List[A] = o match {
      case Nil => n
      case Cons(h, t) => flattenAcc(t, append2(n, h))
    }
    flattenAcc(l, Nil)
  }

  def flatten2[A](l: List[List[A]]): List[A] =
    foldRight2(l, Nil:List[A])(append2(_,_))

  def add1(l: List[Int]): List[Int] =
    foldRight2(l, Nil:List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    def mapAcc(l: List[A], acc: List[B]): List[B] = l match {
      case Nil => acc
      case Cons(h, t) => mapAcc(t, Cons(f(h), acc))
    }
    mapAcc(reverse(l), Nil)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def filterAcc[A](o: List[A], n: List[A])(f: A=> Boolean): List[A] = o match {
      case Nil => n
      case Cons(h, t) => if (f(h)) filterAcc(t, Cons(h, n))(f) else filterAcc(t, n)(f)
    }
    filterAcc(reverse(l), Nil)(f)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = {
    @annotation.tailrec
    def flatMapAcc[A, B](o: List[A], n: List[B])(f: A => List[B]): List[B] = o match {
      case Nil => n
      case Cons(h, t) => flatMapAcc(t, append(f(h), n))(f)
    }
    flatMapAcc(reverse(l), Nil)(f)
  }

  def filter3[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap2(l)((x) => if (f(x)) Cons(x, Nil) else Nil)

  def zipAdd(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, Nil) => Nil
    case (Nil, l) => l
    case (l, Nil) => l
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha+hb, zipAdd(ta, tb))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
    def zipWithAcc(a: List[A], b: List[A], r: List[A]): List[A] = (a, b) match {
      case (Nil, Nil) => r
//      case (Nil, l) => append(reverse(l), r)
//      case (l, Nil) => append(reverse(l), r)
      case (Nil, _) => throw new IndexOutOfBoundsException("List b is longer than list a")
      case (_, Nil) => throw new IndexOutOfBoundsException("List a is longer than list b")
      case (Cons(ah, at), Cons(bh, bt)) => zipWithAcc(at, bt, Cons(f(ah, bh), r))
    }
    reverse(zipWithAcc(a, b, Nil))
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(ha, ta), Cons(hb, tb)) => (ha == hb) && startsWith(ta, tb)
    }
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(ha, ta), Cons(hb, tb)) => ((ha == hb) && startsWith(ta, tb)) || hasSubsequence(ta, sub)
    }
  }
}
