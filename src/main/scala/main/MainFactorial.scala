package main

import java.time.Instant

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

  def schemaOptimize[A](baseCase: A, rec: (Int, A) ⇒ A)(n: Int): A = {
    var res = baseCase
    var i = 1
    while (i <= n) {
      res = rec(i, res)
      i += 1
    }
    res
  }

  def time[T](f: ⇒ T): T = {
    val t0 = Instant.now.toEpochMilli
    val res = f
    println(s"Time: ${Instant.now.toEpochMilli - t0} ms")
    res
  }

  def factorial(n: Int): Int = time(schema[Int](1, _ * _)(n))
  def factorialOptimize(n: Int): Int = time(schemaOptimize[Int](1, _ * _)(n))
  def sum(n: Int): Int = schema[Int](0, _ + _)(n)
  def isPair(n: Int) = schema[Boolean](true, (_, b: Boolean) ⇒ !b)(n)

  //  val integersList = schema[Seq[Int]](Nil, (i, seq) ⇒ seq :+ i)(10)
  //  val stringConcat = schema[Seq[String]](Nil, (i, seq) ⇒ i.toString +: seq)(10)
  //  val factorial4 = factorial(4)
  //  val sum4 = sum(4)
  //  val isPair3 = isPair(3)
  //
  //  println(factorial4)
  //  println(sum4)
  //  println(isPair3)

  println("basic")
  println(factorial(25))

  println("optimize")
  println(factorialOptimize(25))

}
