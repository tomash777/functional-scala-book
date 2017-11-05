package chapter3

object maps extends App{
  
  def map[A,B] (l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case x::xs => f(x) :: map(xs)(f)
  }
  
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
  
  def map1[A,B] (l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((x, l1) => f(x)::l1)
  
  def filter[A] (l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x::xs => if (p(x)) xs else l
  }
  
  def flatMap[A,B] (l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case x::xs => f(x) ++ flatMap(xs)(f)
  }

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = flatMap (l)(x => if(p(x)) Nil else List(x))

  def combineLists[A,B,C] (l1: List[A], l2: List[B])(combine: (A,B) => C): List[C] = l1 match{
    case Nil => Nil
    case x::xs => combine(x,l2.head) :: combineLists(xs, l2.tail)(combine)
  }
  
  def take[A] (l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case x::xs => if (n==0) Nil else x :: take(xs, n-1)
  }
  
  def takeWhile[A] (l: List[A])(f: A => Boolean): List[A] =l match {
    case Nil => Nil
    case x::xs => if (f(x)) x::takeWhile(xs)(f) else Nil
  }
  
  def forall[A] (l: List[A])(f: A => Boolean): Boolean = l match {
    case Nil => true
    case x::xs => f(x) && forall(xs)(f)
    }
  
  def exists[A] (l: List[A])(f: A => Boolean): Boolean = !forall(l)(!f(_))
}