package main

import matryoshka.data.Fix
import matryoshka.{ Algebra, Coalgebra }
import model.{ Doll, Matryoshka, Tiny }
import scalaz.{ Functor, INil, NonEmptyList }
import matryoshka.implicits._

object MainMatryoshka extends App {

  /*
   Avoid to Matryoshka[Matryoashka[Matryoashka.....Matryoashka[Nothing]]]

    */
  val dolls: Fix[Matryoshka] = Fix(Doll("Anna", Fix(Doll("Cata", Fix(Tiny("Hylo"))))))

  implicit val dollsFunctor: Functor[Matryoshka] = new Functor[Matryoshka] {
    override def map[A, B](fa: Matryoshka[A])(f: A ⇒ B): Matryoshka[B] = fa match {
      case Tiny(n)    ⇒ Tiny(n)
      case Doll(n, d) ⇒ Doll(n, f(d))
    }
  }
  val algebra: Algebra[Matryoshka, Int] = {
    case Doll(_, d) ⇒ 1 + d
    case Tiny(_)    ⇒ 1
  }

  /*
   Construct a Matryoshka from a NonEmptyList[String], ie: A=>F[A]
   with A=NonEmptyList[String] and F=Matryoashka
    */
  val coalgeabra: Coalgebra[Matryoshka, NonEmptyList[String]] = {
    case NonEmptyList(h, _: INil[String]) ⇒ Tiny(h)
    case NonEmptyList(h, l)               ⇒ Doll(h, NonEmptyList(l.toList.head, l.toList.tail: _*))
  }

  val names = NonEmptyList("anna", "cata", "hylo", "okok", "toto")
  val res: Fix[Matryoshka] = names.ana[Fix[Matryoshka]](coalgeabra)

  println(res)

  dolls.cata[Int](algebra)

  val res2 = dolls.cata[Int](algebra)

  println(names.hylo(algebra, coalgeabra))

  //  println(res2)
}
