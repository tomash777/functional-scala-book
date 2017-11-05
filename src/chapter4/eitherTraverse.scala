package chapter4

object eitherTraverse extends App {
  
  def map[A,B,E] (x: Either[E,A])(f: A => B): Either[E,B] = x match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  
  def flatMap[A, E, B](x: Either[E,A])(f: A => Either[E,B]): Either[E,B] = x match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  
  def map2[E, A, B, C](a: Either[E, A], b: Either[E,B])(f: (A,B) => C): Either[E,C] =
   flatMap (a)(y => (map (b)(x => f(y,x))))
   
   def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
   
   def traverse[A,B,E](a: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    foldRight(a, Right(Nil): Either[E, List[B]])((x,l) => map2(f(x),l)(_ :: _))
  
   def sequence[A,E](a: List[Either[E,A]]): Either[E, List[A]] = 
     traverse(a)(x => x)
     
     printf(sequence(List(Right(2),Right(3),Right(4))).toString)
    printf(sequence(List(Right(2),Left("lol"),Right(4))).toString)
}