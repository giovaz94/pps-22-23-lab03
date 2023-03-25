package u03

import org.junit.*
import org.junit.Assert.*
import Functions.*
import Functions.List.*
import u03.Functions.Person.*

class PersonTest:

  var teacher1 = Teacher("Mario Rossi", "Algoritmi e strutture dati")
  var teacher2 = Teacher("Beatrice Fuffa", "Analisi 2")
  var teacher3 = Teacher("Mario Paio", "Basi di dati")
  var student1 = Student("Luca lu", 1994)
  var student2 = Student("Valerio Perio", 2001)

  var personList = Cons(teacher1, Cons(teacher2, Cons(student1, Cons(student2, Cons(teacher3, Nil())))))


  @Test def testListCourses() =
    val outputList = Cons("Algoritmi e strutture dati", Cons("Analisi 2", Cons("Basi di dati", Nil())))
    assertEquals(outputList, listCourses(personList))


