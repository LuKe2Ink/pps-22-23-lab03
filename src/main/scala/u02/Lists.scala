package u02

import u02.AlgebraicDataTypes.Person


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

    //Task part 1, svolto da solo
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(h, t), n) if n > 0 => drop(t, n-1)

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case (Cons(h, t), left) => append(t, left) match
        case l => Cons(h, l)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def filterFlat[A](l: List[A])(pred: A => Boolean): List[A] = l match
      case Nil() => Nil()
      case l => flatMap(l)(x => x match
        case x if pred(x) => Cons(x, Nil())
        case x if !pred(x) => Nil()
      )

    def mapFlat[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Nil() => Nil()
      case l => flatMap(l)(h => Cons(mapper(h), Nil()))

    import u02.Optionals.*;

    def max(l: List[Int]): Option[Int] = l match
      case Nil() => Option.None()
      case Cons(h, Nil()) => Option.Some(h)
      case Cons(h, t) if t!=Nil() => max(t) match
        case Option.Some(x) if x < h => Option.Some(h)
        case Option.Some(x) if x >= h => Option.Some(x)
        case Option.None() => Option.None()

    //Task part 2, svolto da solo

    //get only teachers' courses
    def getTeachersCourses(l: List[Person]): List[String] =
      flatMap(l)(p => p match
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )

    def foldLeft(l: List[Int])(d: Int)(f: (Int, Int) => Int): Int = l match
      case Nil() => d
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)


    def foldRight(l: List[Int])(d: Int)(f: (Int, Int) => Int): Int = l match
      case Nil() => d
      case Cons(h, t) => f(h, foldRight(t)(d)(f))

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  val tail = List.Cons(40, List.Nil())
  println(List.sum(l)) // 60

  import List.*

//  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
//  println(drop(List.Nil(), 1))
//  println(drop(l, 1))
//  println(append(l, tail))
//  println(flatMap(l)(v => Cons(v, Nil())))
//  println(max(Cons(10, Cons(25, Cons(20, Nil())))))
//  println(filterFlat(l)(_ != 30))
//  println(mapFlat(l)(_ + 1))

