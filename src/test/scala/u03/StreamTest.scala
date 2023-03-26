package u03

import org.junit.*
import org.junit.Assert.*
import Streams.*
import Lists.List.*

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




