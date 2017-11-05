package chapter3

object trees1 {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 1
    case Branch(l,r) => size(l)+size(r)
  }
  
  def maximum (tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  def depth[A] (tree: Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(l,r) => (depth(l) max depth(r))+1
  }
  
  def map[A,B] (tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l,f), map(r,f))
  }
  
  def fold[A,B] (tree: Tree[A], f: A => B, g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))
  }
  
  def size1[A] (tree: Tree[A]): Int = fold[A, Int](tree, x => 1, _+_)
  def maximum1 (tree: Tree[Int]): Int = fold[Int,Int] (tree, x => x, _ max _)
  def depth1[A] (tree: Tree[A]): Int = fold[A,Int] (tree, x => 0, (x,y) => (x max y) +1) 
  def map1[A,B] (tree: Tree[A], f: A => B): Tree[B] = fold[A, Tree[B]](tree, x => Leaf(f(x)), Branch(_,_))
  
  
}