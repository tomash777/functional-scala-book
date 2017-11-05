package chapter4

object eithers1 {
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = flatMap (x => Right(f(x)))
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => b
      case Right(v) => Right(v)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
      flatMap (a => b map (x => f(a,x)))
  }
  
  def foldRight[A,B] (l: Seq[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
  
  def traverse[A,B,E](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = 
    foldRight(as, Right(Nil): Either[E,List[B]])((x,y) => f(x).map2 (y)(_::_))
    
  def sequence[A,E](as:List[Either[E,A]]): Either[E,List[A]] = traverse(as)(x => x)  
    
}