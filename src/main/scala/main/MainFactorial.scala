package main

object MainFactorial extends App {

  def scheme(case0: Int, rec: (Int, Int) => Int)(n: Int): Int = {
    if(n == 0) case0
    else {
      val r = scheme(case0, rec)(n-1)
      rec(n, r)
    }
  }

  def factorial(n: Int): Int = scheme(1, _*_)(n)
  def sum(n: Int): Int = scheme(0, _+_)(n)



  def schemaGen[A](case0: A, rec: (Int, A) => A)(n: Int): A = {
    if(n == 0) case0
    else {
      val r = schemaGen(case0, rec)(n-1)
      rec(n, r)
    }
  }

  schemaGen[Seq[Int]](Nil, (i, seq) => seq :+ i)(10).foreach(println)
  schemaGen[Seq[String]](Nil, (i, seq) => i.toString +: seq)(10).foreach(println)


}
