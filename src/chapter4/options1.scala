package chapter4


object options1 extends App{
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }
    
    def map1[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))
    
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }
       
    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
      
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
    
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (x => (b map (y => f(x,y))))
  
  def foldRight[A,B] (l: Seq[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = foldRight[Option[A],Option[List[A]]](a, Some(Nil))((x,xs)=>map2(x,xs)(_::_))

      printf(sequence(List(Some(2),Some(3),Some(4))).toString+"\n")
    printf(sequence(List(Some(2),None,Some(4))).toString+"\n")  
  
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)
  
  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = foldRight(a,Some(Nil):Option[List[B]])((x,xs) => map2(f(x),xs)(_::_))
  
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = traverse1[Option[A],A](a)(x=>x)
  
   printf(sequence1(List(Some(2),Some(3),Some(4))).toString+"\n")
    printf(sequence1(List(Some(2),None,Some(4))).toString+"\n")
  
  def variance(xs: Seq[Double]): Option[Double] = 
      sequence(xs.toList map (x => map2(Some(x),mean(xs))((x,y) => math.pow(x-y,2)))) flatMap (mean)
      
  printf(variance(Nil).toString + "\n")
  printf(variance(List(1.0,2.0,3.0)).toString + "\n")

                    
}