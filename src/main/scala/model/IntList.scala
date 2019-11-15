package model

sealed trait IntList[T]
case class Empty[T]() extends IntList[T]
case class Cons[T](head: Int, tail: T) extends IntList[T]
