package u03

import u02.Optionals.Option
import u02.Optionals.Option.*
import scala.annotation.tailrec


object Functions extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()


    // Task01: drop --> svolto da solo
    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(a, t), 0) => Cons(a, t)
      case (Nil(), _) => Nil()
      case (Cons(_, t), _) => drop(t, n - 1)

    // Task01: append --> svolto da solo
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case (Cons(h, t), _) => Cons(h, append(t, right))

    // Task01: flatmap --> svolto da solo
    def flatMap[A,B](lst: List[A])(f: A => List[B]): List[B] = lst match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    // Task01: mapWithFlatMap --> svolto da solo
    def mapWithFlatMap[A, B](lst: List[A])(f: A => B): List[B] = lst match
      case _ => flatMap(lst)(v => Cons(f(v), Nil()))

    // Task01: filterWithFlatMap --> svolto da solo
    def filterWithFlatMap[A](lst: List[A])(pred: A => Boolean): List[A] = lst match
      case _ => flatMap(lst)(x => pred(x) match
        case true => Cons(x, Nil())
        case false => Nil())

    // Task01: max --> svolto da solo
    def max(lst: List[Int]): Option[Int] = lst match
      case Nil() => None()
      case Cons(h, t) => max(t) match
        case None() => Some(h)
        case Some(v) => Some(h max v) // h.max(v)


  // Task02: listCourses --> svolto da solo
  import Functions.List.*

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    def listCourses(lst: List[Person]): List[String] = lst match
      case _ => flatMap(lst)(x => x match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil())


  // Task02: foldLeft --> svolto da solo
  @tailrec
  def foldLeft[A, B <: A](lst: List[A])(accumulator: B)(f: (x: B, y: A) => B): A = lst match
    case Nil() => accumulator
    case Cons(h, t) => foldLeft(t)(f(accumulator, h))(f)

  // Task02: foldRight --> svolto da solo
  @tailrec
  def reverseList[A](lst: List[A], b: List[A]): List[A] = lst match
    case Cons(h, t) => reverseList(t, append(Cons(h, Nil()), b))
    case Nil() => b

  def foldRight[A, B <: A](lst: List[A])(accumulator: B)(f: (x: A, y: B) => B): A = lst match
    case _ => foldLeft(reverseList(lst, Nil()))(accumulator)((a, x) => f(x, a))


  object Streams extends App:

    enum Stream[A]:
      private case Empty()
      private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

      def empty[A](): Stream[A] = Empty()

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

      // Task03: drop --> svolto da solo
      @tailrec
      def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
        case Cons(_, t) if (n > 0) => drop(t())(n - 1)
        case _ => stream

      // Task03: constant --> svolto da solo
      def constant[A](value: => A): Stream[A] = value match
        case _ => cons(value, constant(value))

      // Task03: fibs --> svolto da solo
      def fibs: Stream[Int] =
        def innerFibonacci(v: => Int): Int = v match
          case 0 => 0
          case 1 => 1
          case _ => innerFibonacci(v - 1) + innerFibonacci(v - 2)

        Stream.map(Stream.iterate(0)(_ + 1))(innerFibonacci)

    end Stream


