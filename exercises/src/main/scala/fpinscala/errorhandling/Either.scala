package fpinscala.errorhandling


//import fpinscala.errorhandling.{Left, Right}

import scala.util.Try
import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => try Right(f(a)) catch { case e: E => Left(e) }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  this flatMap (aa => b map(bb => f(aa, bb)))

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(List())
    case (h :: t) => f(h) match {
      case Left(e1) => Left(e1)
      case Right(b) => traverse(t)(f) match {
        case Left(e2) => Left(e2)
        case Right(r) => Right(b::r)
      }
    }
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match  {
    case Nil => Right(Nil)
    case (h :: t) => h match {
      case Left(e1) => Left(e1)
      case Right(a) => sequence(t) match {
        case Left(e2) => Left(e2)
        case Right(r) => Right(a::r)
      }
    }
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}