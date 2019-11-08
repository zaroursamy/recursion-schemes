import scalaz.Functor
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._


sealed trait Nat[T]
case class Zero[T]() extends Nat[T]
case class Succ[T](t: T) extends Nat[T]

object Nat extends App {

  implicit val functorNat: Functor[Nat] = new Functor[Nat] {
    override def map[A, B](fa: Nat[A])(f: A => B): Nat[B] = fa match {
      case Zero() => Zero[B]()
      case Succ(a) => Succ(f(a))
    }
  }

  val algebraNat: Algebra[Nat, Int] = {
    case Zero() => 0
    case Succ(i) => i+1
  }

  val coalgebraNat: Coalgebra[Nat, Int] = {
    case 0 => Zero()
    case i => Succ(i-1)
  }

  def multiply[T](m: Nat[T], n: Nat[T])(op: (T,T)=>T): Nat[T] = {
    (m,n) match {
      case (Zero(), _) | (_, Zero()) => Zero[T]()
      case (Succ(mm), Succ(nn)) => Succ(op(mm,nn))
    }
  }

  println(multiply(Succ(0), Succ(3))(_*_))
  println(multiply(Succ(2), Succ(3))(_*_))

  val res: Fix[Nat] = Fix(Succ(Fix(Succ(3))))

  println(res.cata(algebraNat))
}