package tasks

import org.junit.*
import org.junit.Assert.*
import Lab03.*
import tasks.Lab03.Stream.fibs
import u03.Lists.*

class lab03Test {
  import List.*
  import Person.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Nil(), drop(Nil(), 0))
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 0))
    assertEquals(Cons(10, Cons(30, Nil())), drop(l, 1))
    assertEquals(l, drop(l, -1))

  @Test def testAppend() =
    assertEquals(l, append(l, Nil()))
    assertEquals(Cons(0, l), append(Cons(0, Nil()), l))
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Nil()))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax() =
    assertEquals(Some(30), max(l))
    assertEquals(Some(25), max(Cons(25, Cons(20, Nil()))))
    assertEquals(None, max(Nil()))

  @Test def testGetCourse() =
    val personList = Cons(Student("A", 1), Cons(Teacher("B", "Z"), Cons(Teacher("C", "Y"), Nil())))
    assertEquals(Cons("Z", Cons("Y", Nil())), getCourse(personList))
    assertEquals(Nil(), getCourse(Nil()))

  @Test def testFoldLeft() =
    assertEquals(-10, foldLeft(l)(50)(_ - _))
    assertEquals(-30, foldRight(l)(50)(_ - _))

  @Test def testDropStream() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstantStrams() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))),
      Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibs() =
    assertEquals(
    Stream.toList(Stream.take(fibs)(8)),
    Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))))


}