package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = {
    def maxAcc(t: Tree[Int], m: (Boolean, Int)): (Boolean, Int) = t match {
      case Leaf(x: Int) => if (m._1) (true, m._2 max x) else (true, x)
      case Branch(l: Tree[Int], r: Tree[Int]) => maxAcc(r, maxAcc(l, m))
    }
    maxAcc(t, (false, 0))._2
  }

  def depth[A](t: Tree[A]): Int = {
    def depthAcc[A](t: Tree[A], m: Int, c: Int): Int = t match {
      case Leaf(_) =>  m max c
      case Branch(l: Tree[A], r: Tree[A]) => depthAcc(r, depthAcc(l, m, c + 1), c + 1)
    }
    depthAcc(t, 0, 0)
  }

  def depth2[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => depth2(l) max depth2(r) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(lf: A => B)(bf: (B, B) => B): B = t match {
    case Leaf(x) => lf(x)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)((_) => 1)(_ + _ + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)((x: Int) => x)((x: Int, y: Int) => if (x > y) x else y)

  def depth3[A](t: Tree[A]): Int =
    fold(t)((_) => 1)((x: Int, y: Int) => x max y + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    def lf[A,B](x: A)(f: A => B): Tree[B] = Leaf(f(x))
    def br[D](x: Tree[D], y: Tree[D]): Tree[D] = Branch(x, y)
    fold(t)(lf(_)(f))(br)
  }
}