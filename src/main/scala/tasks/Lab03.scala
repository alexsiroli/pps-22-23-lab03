package tasks

import u03.Lists.*
import u03.Streams.*
import u03.Streams.Stream.Cons

import scala.annotation.tailrec

object Lab03 extends App {
  import List.*
  import Option.*

  // Task 1
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil() => Nil()
    case Cons(_, t) if n == 0 => t
    case Cons(h, t) => Cons(h, drop(t, n-1))

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Nil(), r) => r
    case (Cons(lh, lt), r) => Cons(lh, append(lt, r))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Nil() => Nil()
    case Cons(h, t) => append(f(h), flatMap(t)(f))

  def map[A, B](l: List[A])(mapper: A => B): List[B] =
    flatMap(l)(v => Cons(mapper(v), Nil()))

  def filter[A](l: List[A])(pred: A => Boolean): List[A] =
    flatMap(l)(v => l match
      case Cons(_, _) if pred(v) => Cons(v, Nil())
      case _ => Nil())

  // Task 2
  @tailrec
  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None
    case Cons(h1, Cons(h2, t)) => max(Cons(Math.max(h1, h2), t))
    case Cons(h, Nil()) => Some(h)

  // Task 3
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  import Person.*

  def getCourse(l: List[Person]): List[String] = flatMap(l)(s => s match
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil())

  // Task 4
  @tailrec
  def foldLeft[A, B](l: List[A])(d: B)(o: (B, A) => B): B = l match
    case Cons(h, t) => foldLeft(t)(o(d, h))(o)
    case _ => d

  def foldRight[A, B](l: List[A])(d: B)(o: (A, B) => B): B = l match
    case Cons(h, t) => o(h, foldRight(t)(d)(o))
    case _ => d

  // Strams tasks
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
      case (s, n) if n == 0 => s
      case _ => Empty()

    // Task 6
    def constant[A](e: A): Stream[A] = cons(e, constant(e))

    // Task 7
    private def fib(n1: Int)(n2: Int): Stream[Int] = cons(n1, fib(n2)(n1 + n2))
    def fibs: Stream[Int] = fib(0)(1)

  end Stream


}
