package model

sealed trait Dir[T]

case class HighLevel[T](name: String, subLevel: T) extends Dir[T]
case class EndLevel[T](name: String) extends Dir[T]