package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 3))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    val tail = Cons (40 , Nil () )
    assertEquals(Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ())))), append(l , tail))

  @Test def testFlatMap() =
    val f: Int => List[Int] = v => Cons(v * 2, Nil())
    val g: Int => List[Boolean] = v => Cons(v <= 20, Nil())
    val e: Int => List[Int] = v => Cons(v + 1, Cons(v + 2, Nil()))

    assertEquals(Cons(20, Cons(40, Cons(60, Nil()))), flatMap(l)(f))
    assertEquals(Cons(true, Cons(true, Cons(false, Nil()))), flatMap(l)(g))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(e))

  @Test def testMapAsFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def filterAsFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))
