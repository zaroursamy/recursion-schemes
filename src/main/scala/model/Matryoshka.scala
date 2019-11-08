package model

sealed trait Matryoshka[T]
case class Doll[T](name: String, daughter: T) extends Matryoshka[T]
case class Tiny[T](name: String) extends Matryoshka[T]