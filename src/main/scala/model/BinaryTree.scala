package model

sealed trait BinaryTree[T]
case class Node[T](left: T, right: T) extends BinaryTree[T]
case class Leaf[T](value: Int) extends BinaryTree[T]

