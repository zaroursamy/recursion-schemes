package testing

sealed trait SchemaR[T]
final case class StructR[T](fields: Map[String, T]) extends SchemaR[T]
final case class ArrayR[T](element: T) extends SchemaR[T]
final case class IntR[T](nullable: Boolean = false) extends SchemaR[T]
final case class StringR[T](nullable: Boolean = false) extends SchemaR[T]
final case class LongR[T](nullable: Boolean = false) extends SchemaR[T]
final case class DoubleR[T](nullable: Boolean = false) extends SchemaR[T]
final case class BooleanR[T](nullable: Boolean = false) extends SchemaR[T]
