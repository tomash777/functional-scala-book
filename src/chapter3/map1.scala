package chapter3

object map1 extends App{
  
  def map[A,B] (list: List[A])( f: A => B): List[B] = list match {
    case Nil => Nil
    case x::xs => f(x)::map(xs)(f)
  }
  
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
  
  def map1[A,B] (list:List[A])(f:A => B): List[B] = foldRight (list, Nil: List[B])(f(_)::_)
  
  def filter[A] (list: List[A], p: A => Boolean): List[A] = list match {
    case Nil => Nil
    case x::xs => if(p(x)) x::filter(xs,p) else filter(xs,p)
  }
  
  def flatMap[A,B] (list: List[A])(f: A => List[B]): List[B] = list match {
    case Nil => Nil
    case x::xs => f(x)++flatMap(xs)(f)
  }
  
  def filter1[A] (list: List[A], p: A => Boolean): List[A] = 
    flatMap (list)(x => if(p(x)) List(x) else Nil)
    
  def combineLists[A,B,C] (l1: List[A], l2: List[B])(combine: (A,B) => C): List[C] = (l1,l2) match {
    case (x::xs,y::ys) => combine(x,y)::combineLists(xs,ys)(combine)
    case (x::xs, Nil) => throw new IndexOutOfBoundsException
    case (Nil, y::ys) => throw new IndexOutOfBoundsException
    case (Nil,Nil) => Nil
  }
  
    def take[A] (l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case x::xs => if(n==0) Nil else x::take(xs,n-1)
    }
    
  def takeWhile[A] (l: List[A])(f: A => Boolean): List[A] =l match {
    case Nil => Nil
    case x::xs => if(f(x)) x::takeWhile(xs)(f) else Nil
  }

  def forall[A] (l: List[A])(f: A => Boolean): Boolean = l match {
    case Nil => true
    case x::xs => f(x) && forall(xs)(f)
  }
  
  def exists[A] (l: List[A])(f: A => Boolean): Boolean = !forall(l)(x => !f(x))

  
}