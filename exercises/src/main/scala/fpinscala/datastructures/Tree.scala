package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case (_: Leaf[A]) => 1
    case (Branch(left, right)) => size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case (l: Leaf[Int]) => l.value
    case (Branch(left, right)) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case (_: Leaf[A]) => 1
    case (Branch(left, right)) => (depth(left) max depth(right)) + 1
  }

  def mapTree[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case (l: Leaf[A]) => Leaf(f(l.value))
    case (Branch(left, right)) => Branch(mapTree(left)(f), mapTree(right)(f))
  }

  def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B)(g: (B, B) => B): B = t match {
    case (l: Leaf[A]) => f(l.value, z)
    case (Branch(left, right)) => g(fold(left, z)(f)(g), fold(right, z)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t, 0)((_, _) => 1)(_ + _)
  def maximumViaFold(t: Tree[Int]): Int = fold(t, 0)((leafValue, totalSize) => leafValue + totalSize)(_ max _)
  def depthViaFold[A](t: Tree[A]): Int = fold(t, 0)((_, totalSize) => totalSize + 1)((left, right) => (left max right) + 1)
}