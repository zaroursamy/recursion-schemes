package main

import matryoshka.data.Fix
import matryoshka.{Algebra, Coalgebra}
import model.{BinaryTree, Leaf, Node}
import scalaz.{Functor, INil, NonEmptyList}

object MainBinaryTree extends App {

  val binaryTree: Fix[BinaryTree] = Fix(Node(Fix(Leaf(4)), Fix(Node(Fix(Leaf(10)), Fix(Leaf(11))))))

  implicit val binaryTreeFunctor: Functor[BinaryTree] = new Functor[BinaryTree] {
    override def map[A, B](fa: BinaryTree[A])(f: A ⇒ B): BinaryTree[B] = fa match {
      case Leaf(v)    ⇒ Leaf(v)
      case Node(l, r) ⇒ Node(f(l), f(r))
    }
  }

  val coalgebra: Coalgebra[BinaryTree, NonEmptyList[Int]] = {
    case NonEmptyList(head, _: INil[Int])      ⇒ Leaf(head)
    case NonEmptyList(head, tail) if head < 10 ⇒ Node(NonEmptyList(head), NonEmptyList(tail.toList.head, tail.toList.tail: _*))
    case NonEmptyList(head, tail)              ⇒ Node(NonEmptyList(tail.toList.head, tail.toList.tail: _*), NonEmptyList(head))
  }

  import matryoshka.implicits._
  println(NonEmptyList(1, 2, 3).ana[Fix[BinaryTree]](coalgebra))
  println(NonEmptyList(10, 50, 30).ana[Fix[BinaryTree]](coalgebra))
  println(NonEmptyList(10, 2, 3).ana[Fix[BinaryTree]](coalgebra))
  println(NonEmptyList(10).ana[Fix[BinaryTree]](coalgebra))

  // Calculer la somme total d'un arbre
  val algebra: Algebra[BinaryTree, Int] = {
    case Leaf(v)    ⇒ v
    case Node(l, r) ⇒ l + r
  }

  // Calculer le nombre de noeuds
  val algebra2: Algebra[BinaryTree, Int] = {
    case Leaf(v)    ⇒ 1 + v
    case Node(l, r) ⇒ 2
  }

  val sum = binaryTree.cata[Int](algebra)
  val sum2 = binaryTree.cata[Int](algebra2)

  println(s"sum=$sum")
  println(s"sum2=$sum2")

  val sumHylo = NonEmptyList(10, 50, 30).hylo(algebra, coalgebra)

  println(sumHylo)

}
