package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // exercise 3.25
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
}

