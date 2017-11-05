package chapter3

trait drop1 {
  def drop[A](l: List[A], n: Int): List[A] = (l,n) match {
    case (Nil,_) => Nil
    case (xs,0) => xs
    case (x::xs,i) => drop(xs,i-1)
  }
  
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x::xs => if(f(x)) dropWhile(xs)(f) else l
  }
  
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x::Nil => Nil
    case x::xs => x::init(xs)
  }
}