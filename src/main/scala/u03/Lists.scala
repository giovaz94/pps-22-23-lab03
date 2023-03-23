package u03

import scala.annotation.tailrec


object Lists extends App :

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
        case true => Cons(x, Nil()),
        case false => Nil()
      )


  import Lists.List.*

  val l = Cons(10, Cons(20, Cons(20, Cons(30, Nil()))))
  val e: Int => List[Int] = v => Cons(v + 1, Cons(v + 2, Nil()))


  println(drop(l,1))
  //println(filterWithFlatMap(l)(_ >= 20))

  /*
  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  val e: Int => List[Int] = v => Cons(v + 1, Cons(v + 2, Nil()))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(e(3))*/