package main

import matryoshka.{ Algebra, Coalgebra }
import matryoshka.data.Fix
import model._
import scalaz.Functor

object MainIntList extends App {

  import matryoshka.implicits._

  implicit val listFunc = new Functor[IntList] {
    override def map[A, B](fa: IntList[A])(f: A ⇒ B): IntList[B] = fa match {
      case Empty()                  ⇒ Empty[B]()
      case Cons(head: Int, tail: A) ⇒ Cons(head, f(tail))
    }
  }

  val coalgebraInt: Coalgebra[IntList, Int] = {
    case 0 ⇒ Empty()
    case n ⇒ Cons(n, n - 1)
  }

  val coalgebraListInt: Coalgebra[IntList, List[Int]] = {
    case Nil      ⇒ Empty()
    case x :: Nil ⇒ Cons(x, Nil)
    case x :: xs  ⇒ Cons(xs.last, x +: xs.reverse.tail.reverse)
  }

  val algebraProduct: Algebra[IntList, Int] = {
    case Empty()          ⇒ 1
    case Cons(head, tail) ⇒ head * tail
  }

  val algebraSum: Algebra[IntList, Int] = {
    case Empty()          ⇒ 0
    case Cons(head, tail) ⇒ head + tail
  }

  val algebraBool: Algebra[IntList, Boolean] = {
    case Empty()       ⇒ true
    case Cons(_, tail) ⇒ !tail
  }

  val algebraList: Algebra[IntList, List[Int]] = {
    case Empty()          ⇒ Nil
    case Cons(head, tail) ⇒ head +: tail
  }

  def factorial(n: Int) = n.hylo(algebraProduct, coalgebraInt)
  def sum(n: Int) = n.hylo(algebraSum, coalgebraInt)
  def even(n: Int) = n.hylo(algebraBool, coalgebraInt)
  factorial(4) // 24
  sum(4) // 10
  even(4) // true

}
