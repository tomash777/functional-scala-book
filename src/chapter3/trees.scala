package chapter3

object trees {
  sealed trait Tree[+A]
  case class Leaf[A] (value: A) extends Tree[A]
  case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]
  
  def size[A] (t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(l, r) => size(l) + size(r)
  }
  
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  
  def depth[A] (t: Tree[A]): Int = t match {
    case Leaf(x) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }
  
  def map[A,B] (t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B] (t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  
  def size1[A] (t: Tree[A]): Int = fold(t)(x => 1)(_+_)
  def maximum1 (t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depth1[A] (t: Tree[A]): Int = fold(t)(x => 0)(_.max(_)+1)
  /**def map1[A,B] (t: Tree[A])(f:A => B): Tree[B] = fold(t)(x => Leaf(f(x)))((x,y) => Branch(map1(x)(f),map1(y)(f)))
  */ }
  