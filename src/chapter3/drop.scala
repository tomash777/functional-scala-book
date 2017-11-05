package chapter3

object drop {
  def drop[A] (n: Int, l: List[A]): List[A] = l match {
    case x::xs => if (n==0) l else drop(n-1, l.tail)
    case Nil => l
  }
  
  def dropWhile[A] (l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case x::xs => if (f(x)) dropWhile(xs)(f) else l
  }
  
  def init[A] (l: List[A]): List[A] = l match {
    case Nil => l
    case List(x) => Nil
    case x::y::xs => x::init(y::xs)
  }
    
}