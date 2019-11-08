package model

sealed trait Nat[T]
case class Zero[T]() extends Nat[T]
case class Succ[T](t: T) extends Nat[T]