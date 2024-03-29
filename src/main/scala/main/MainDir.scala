package main

import matryoshka.data.Fix
import matryoshka.{ Algebra, Coalgebra }
import model.{ Dir, EndLevel, HighLevel }
import scalaz.Functor
import matryoshka.implicits._

object MainDir extends App {

  implicit val dirFunctor: Functor[Dir] = new Functor[Dir] {
    override def map[A, B](fa: Dir[A])(f: A ⇒ B): Dir[B] = fa match {
      case EndLevel(name)            ⇒ EndLevel[B](name)
      case HighLevel(name, subLevel) ⇒ HighLevel(name, f(subLevel))
    }
  }

  val coalgebraSeq: Coalgebra[Dir, Seq[String]] = {
    case Nil      ⇒ EndLevel("/")
    case x :: Nil ⇒ EndLevel(x)
    case x :: xs  ⇒ HighLevel(x, xs)
  }

  val algebraSeq: Algebra[Dir, Seq[String]] = {
    case EndLevel(name)        ⇒ name :: Nil
    case HighLevel(name, subs) ⇒ name +: subs
  }

  val algebraStr: Algebra[Dir, String] = {
    case EndLevel(name)        ⇒ s"/$name"
    case HighLevel(name, subs) ⇒ s"/$name$subs"
  }

  val algebraDepth: Algebra[Dir, Int] = {
    case EndLevel(_)            ⇒ 1
    case HighLevel(_, subLevel) ⇒ 1 + subLevel
  }

  val path: Seq[String] = Seq("home", "ubuntu", "Documents", "doc.pdf")

  val pathDir: Fix[Dir] = Fix(HighLevel("tabmo-data-team", Fix(HighLevel("zeotap", Fix(EndLevel("aa156584.gz"))))))
  val unfixPathDir: Dir[Fix[Dir]] = pathDir.unFix
  println(unfixPathDir)
  // HighLevel(tabmo-data-team,Fix(HighLevel(zeotap,Fix(EndLevel(aa156584.gz)))))

  System exit 0

  // catamorphisme: déconstruire la structure
  val length: Int = pathDir.cata(algebraDepth)
  val list: Seq[String] = pathDir.cata(algebraSeq)
  val str: String = pathDir.cata(algebraStr)

  println(length)
  println(list)
  println(str)

  println("--------")
  println(path.hylo(algebraStr, coalgebraSeq))
  println(path.hylo(algebraSeq, coalgebraSeq))
  println(path.hylo(algebraDepth, coalgebraSeq))

  println(Seq.empty[String].hylo(algebraStr, coalgebraSeq))
  println(Seq.empty[String].hylo(algebraSeq, coalgebraSeq))
  println(Seq.empty[String].hylo(algebraDepth, coalgebraSeq))
}
