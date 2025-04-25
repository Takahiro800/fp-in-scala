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

  // exercise 3.26
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(value)         => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  // exercise 3.27
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }
}

