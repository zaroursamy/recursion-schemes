package model

sealed trait Dir[T]{
  def name: String
}

case class HighLevel[T](name: String, subLevel: T) extends Dir[T]
case class EndLevel[T](name: String) extends Dir[T]