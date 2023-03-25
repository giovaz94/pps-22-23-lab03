package u03

import scala.annotation.tailrec
import scala.jdk.Accumulator


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

    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(a, t), 0) => Cons(a, t)
      case (Nil(), _) => Nil()
      case (Cons(_, t), _) => drop(t, n - 1)

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case (Cons(h, t), _) => Cons(h, append(t, right))

    def flatMap[A,B](lst: List[A])(f: A => List[B]): List[B] = lst match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def mapWithFlatMap[A, B](lst: List[A])(f: A => B): List[B] = lst match
      case _ => flatMap(lst)(v => Cons(f(v), Nil()))

    def filterWithFlatMap[A](lst: List[A])(pred: A => Boolean): List[A] = lst match
      case _ => flatMap(lst)(x => pred(x) match
        case true => Cons(x, Nil())
        case false => Nil()
      )

  import Functions.List.*

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    def listCourses(lst: List[Person]): List[String] = lst match
      case _ => flatMap(lst)(x => x match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil())

/**
 *  foldLeft (lst, accumulator)(op) = lst match
 *    case Cons(h, t) => foldLeft(t, op(h, accumulator))(op)
 *    case Nil() => accumulator
 */
  @tailrec
  def foldLeft[A](lst: List[A])(accumulator: A)(f: (x:A, y:A) => A): A = lst match
    case Nil() => accumulator
    case Cons(h, t) => foldLeft(t)(f(accumulator, h))(f)

  @tailrec
  def reverseList[A](lst: List[A], b: List[A]): List[A] = lst match
    case Cons(h, t) => reverseList(t, append(Cons(h, Nil()), b))
    case Nil() => b

  def foldRight[A](lst: List[A])(accumulator: A)(f: (x:A, y:A) => A): A = lst match
    case _ => foldLeft(reverseList(lst, Nil()))(accumulator)(f)



  var l: List[Int] = Cons(3, Cons(7, Nil()))
  println(foldLeft(l)(0)((x: Int, y: Int) => x - y))
