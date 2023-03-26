package u03

import org.junit.*
import org.junit.Assert.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*
import u02.Lists.*

class Lab03Test:

  import List.*
  import Person.*
  import Streams.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40, Nil())
  val people = Cons(Person.Student("Luis", 2002), Cons(Person.Teacher("Chiara", "Psicologia"), Cons(Person.Teacher("Alessandra", "Filosofia"), Nil())))
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  val streamList = Stream.take(Stream.iterate(0)(_ + 1))(10)


  @Test def testDropList() =
    assertEquals(Cons(20, Cons(30, Nil())), List.drop(l, 1))
    assertEquals(Cons(30, Nil()), List.drop(l, 2))
    assertEquals(Nil(), List.drop(l, 5))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMapWithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapFlat(l)(_ + 1))
    assertEquals(Cons("10L", Cons("20L", Cons("30L", Nil()))), mapFlat(l)(_ + "L"))

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterFlat(l)(_ >= 20))
    assertEquals(Cons(20, Cons(30, Nil())), filterFlat(l)(_ != 10))
    assertEquals(Nil(), filterFlat(l)(_ < 10))

  @Test def testMax() =
    assertEquals(Option.Some(30), max(l))
    assertEquals(Option.Some(7), max(lst))
    assertEquals(Option.None(), max(Nil()))

  @Test def testCourses() =
    assertEquals(Cons("Psicologia", Cons("Filosofia", Nil())), getTeachersCourses(people))
    assertEquals(Nil(), getTeachersCourses(Nil()))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(0, foldLeft(Nil())(0)(_ - _))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(16, foldRight(lst)(0)(_ + _))
    assertEquals(0, foldRight(Nil())(0)(_ - _))

  @Test def testDropStream() =
    assertEquals(Lists.List.Cons(8, Lists.List.Cons(9, Lists.List.Nil())), Stream.toList(Stream.drop(streamList)(8)))

  @Test def testConstantStream() =
    assertEquals(Lists.List.Cons("x", Lists.List.Cons("x", Lists.List.Nil())), Stream.toList(Stream.take(Stream.constant("x"))(2)))

  @Test def testFibonacciStream() =
    assertEquals(Lists.List.Cons(0, Lists.List.Cons(1, Lists.List.Cons(1, Lists.List.Cons(2, Lists.List.Cons(3, Lists.List.Cons(5, Lists.List.Cons(8, Lists.List.Cons(13, Lists.List.Nil())))))))), Stream.toList(Stream.take(Stream.fibs)(8)))
