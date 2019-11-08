import scalaz.Functor
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

sealed trait Dir[T]{
  def name: String
}

case class HighLevel[T](name: String, subLevel: T) extends Dir[T]
case class EndLevel[T](name: String) extends Dir[T]

object Dir extends App {

  implicit val dirFunctor: Functor[Dir] = new Functor[Dir] {
    override def map[A, B](fa: Dir[A])(f: A => B): Dir[B] = fa match {
      case EndLevel(name) => EndLevel[B](name)
      case HighLevel(name, subLevel) => HighLevel(name, f(subLevel))
    }
  }

  val coalgebraSeq: Coalgebra[Dir, Seq[String]] = {
    case Nil => EndLevel("")
    case x::Nil => EndLevel(x)
    case x::xs => HighLevel(x, xs)
  }

  val algebraSeq: Algebra[Dir, Seq[String]] = {
    case EndLevel(name) => name::Nil
    case HighLevel(name, subs) => name+:subs
  }

  val algebraStr: Algebra[Dir, String] = {
    case EndLevel(name) => name
    case HighLevel(name, subs) => s"$name/$subs"
  }

  val algebraLength: Algebra[Dir, Int] = {
    case EndLevel(_) => 1
    case HighLevel(_,subLevel) => 1+subLevel
  }

  val pathDir: Fix[Dir] = Fix(HighLevel("tabmo-data-team", Fix(HighLevel("zeotap", Fix(EndLevel("aa156584.gz"))))))


  val path: Seq[String] = Seq("home","ubuntu","Documents", "doc.pdf")

  println(path.hylo(algebraStr, coalgebraSeq))
  println(path.hylo(algebraSeq, coalgebraSeq))
  println(path.hylo(algebraLength, coalgebraSeq))






}
