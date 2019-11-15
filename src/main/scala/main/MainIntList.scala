package main

import matryoshka.{ Algebra, Coalgebra }
import matryoshka.data.Fix
import model._
import scalaz.Functor

object MainIntList extends App {

  import matryoshka.implicits._

  implicit val listFunc = new Functor[IntList] {
    override def map[A, B](fa: IntList[A])(f: A ⇒ B): IntList[B] = fa match {
      case Empty()          ⇒ Empty[B]()
      case Cons(head, tail) ⇒ Cons(head, f(tail))
    }
  }

  val coalgebraIntList: Coalgebra[IntList, Int] = {
    case 0 ⇒ Empty()
    case n ⇒ Cons(n, n - 1)
  }

  val productIntList: Algebra[IntList, Int] = {
    case Empty()          ⇒ 1
    case Cons(head, tail) ⇒ head * tail
  }

  val sumIntList: Algebra[IntList, Int] = {
    case Empty()          ⇒ 0
    case Cons(head, tail) ⇒ head + tail
  }

  val boolIntList: Algebra[IntList, Boolean] = {
    case Empty()       ⇒ true
    case Cons(_, tail) ⇒ !tail
  }

  val listIntList: Algebra[IntList, List[Int]] = {
    case Empty()          ⇒ Nil
    case Cons(head, tail) ⇒ head +: tail
  }

  val coalList: Coalgebra[IntList, List[Int]] = {
    case Nil      ⇒ Empty()
    case x :: Nil ⇒ Cons(x, Nil)
    case x :: xs  ⇒ Cons(xs.last, x +: xs.reverse.tail.reverse)
  }

  println(4.hylo(listIntList, coalgebraIntList) /*.ana[Fix[IntList]](coalgebraIntList).cata(listIntList)*/ .ana[Fix[IntList]](coalList))
}
