package main

object MainFactorial extends App {

  //  def scheme(case0: Int, rec: (Int, Int) => Int)(n: Int): Int = {
  //    if(n == 0) case0
  //    else {
  //      val r = scheme(case0, rec)(n-1)
  //      rec(n, r)
  //    }
  //  }

  def schema[A](baseCase: A, rec: (Int, A) ⇒ A)(n: Int): A = {
    if (n == 0) baseCase
    else {
      val r = schema(baseCase, rec)(n - 1)
      rec(n, r)
    }
  }

  def factorial(n: Int): Int = schema[Int](1, _ * _)(n)
  def sum(n: Int): Int = schema[Int](0, _ + _)(n)
  def isPair(n: Int) = schema[Boolean](true, (_, b: Boolean) ⇒ !b)(n)

  val integersList = schema[Seq[Int]](Nil, (i, seq) ⇒ seq :+ i)(10)
  val stringConcat = schema[Seq[String]](Nil, (i, seq) ⇒ i.toString +: seq)(10)
  val factorial4 = factorial(4)
  val sum4 = sum(4)
  val isPair3 = isPair(3)

  println(factorial4)
  println(sum4)
  println(isPair3)

}
