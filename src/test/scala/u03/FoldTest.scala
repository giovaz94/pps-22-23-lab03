package u03

import org.junit.*
import org.junit.Assert.*
import Functions.List.*
import Functions.*

class FoldTest :

  val lst: Functions.List[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _ ))


  @Test def testFoldRight() =
    assertEquals(-8, foldRight ( lst ) (0) ( _ - _ ))
    assertEquals(16, foldRight ( lst ) (0) ( _ + _ ))
