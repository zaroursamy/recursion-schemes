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

  def scheme[A](baseCase: A, rec: (Int, A) ⇒ A)(n: Int): A = {
    if (n == 0) baseCase
    else {
      val r = scheme(baseCase, rec)(n - 1)
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

  println(schemaOptimize(None, (i: Int, xs: Option[Int]) ⇒ Some(xs.getOrElse(0) + i))(10)) // 55

  def time[T](f: ⇒ T): T = {
    val t0 = Instant.now.toEpochMilli
    val res = f
    println(s"Time: ${Instant.now.toEpochMilli - t0} ms")
    res
  }

  def factorial(n: Int): Int = time(scheme[Int](1, _ * _)(n))
  def factorialOptimize(n: Int): Int = time(schemaOptimize[Int](1, _ * _)(n))
  def sum(n: Int): Int = scheme[Int](0, _ + _)(n)
  def isPair(n: Int) = scheme[Boolean](true, (_, b: Boolean) ⇒ !b)(n)

}
