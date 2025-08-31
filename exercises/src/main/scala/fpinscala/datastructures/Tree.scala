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

  // exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // excercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(a => 1)(1 + _ + _)
  }

  def maximumViaFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(a => 0)(1 + _ max _)
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

}
