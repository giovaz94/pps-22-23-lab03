package u03

import scala.annotation.tailrec

object Streams extends App :

  import Lists.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def constant[A](value: => A): Stream[A] = value match
      case _ => cons(value, constant(value))

    def fibs: Stream[Int] =
      def innerFibonacci(v: =>Int): Int = v match
        case 0 => 0
        case 1 => 1
        case _ => innerFibonacci(v - 1) + innerFibonacci(v - 2)
      Stream.map(Stream.iterate(0)(_ + 1))(innerFibonacci)


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

    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(_, t) if (n > 0) => drop(t())(n - 1)
      case _ => stream

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

  end Stream

  // var simplifies chaining of functions a bit..
  var str = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  str = Stream.map(str)(_ + 1) // {1,2,3,4,..}
  str = Stream.filter(str)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  str = Stream.take(str)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str)) // [1,2,21,22,..,28]

  val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]