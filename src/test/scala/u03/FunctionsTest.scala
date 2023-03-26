package u03

import org.junit.*
import org.junit.Assert.*
import Functions.*
import Functions.List.*
import Functions.Streams.*
import u02.Optionals.Option.Some
import u03.Functions.Person.{Student, Teacher, listCourses}

class ListTests :

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 3))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

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

  @Test def testFilterAsFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(10), max(Cons(5, Cons(2, Cons(10, Nil())))))


class PersonTest:

  val teacher1 = Teacher("Mario Rossi", "Algoritmi e strutture dati")
  val teacher2 = Teacher("Beatrice Fuffa", "Analisi 2")
  val teacher3 = Teacher("Sergio Lato", "Basi di dati")
  val student1 = Student("Luca lu", 1994)
  val student2 = Student("Valerio Perio", 2001)
  val personList = Cons(teacher1, Cons(teacher2, Cons(student1, Cons(student2, Cons(teacher3, Nil())))))


  @Test def testListCourses() =
    val outputList = Cons("Algoritmi e strutture dati", Cons("Analisi 2", Cons("Basi di dati", Nil())))
    assertEquals(outputList, listCourses(personList))

class StreamTest :

  @Test def testDrop() =
    val stream = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(stream)(6)))

  @Test def testConstant() =
    val stream = Stream.toList(Stream.take(Stream.constant("x"))(5))
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), stream)

  @Test def testFibonacci() =
    val desiredOutput = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil()))))))))
    assertEquals(desiredOutput, Stream.toList(Stream.take(Stream.fibs)(8)))


