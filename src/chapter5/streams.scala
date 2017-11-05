package chapter5

object streams extends App{
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] = uncons match {
      case None => Nil
      case Some((x,y)) => x :: y.toList
    }
    def take(n: Int): Stream[A] = if (n == 0) Stream.empty else uncons match {
      case None => Stream.empty
      case Some((x,y)) => Stream.cons(x, y.take(n-1))
    }
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case None => Stream.empty
      case Some((x,y)) => if (p(x)) Stream.cons(x, y.takeWhile(p)) else y
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case None => z
      case Some((x,y)) => f(x, y.foldRight(z)(f))
    }
    def exists(p: A => Boolean): Boolean =
      foldRight(false)((x,y) => p(x) || y)
   
    def forall(p: A => Boolean): Boolean =
      foldRight(true)((x,y) => p(x) && y)
      
    def takeWhile1(p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty: Stream[A])((x,y) => if (p(x)) Stream.cons(x,y) else Stream.empty)
     
    def map[B] (f: A => B): Stream[B] =
      foldRight(Stream.empty: Stream[B])((x,y) => Stream.cons(f(x), y))
      
    def filter (p: A => Boolean): Stream[A] =
      foldRight(Stream.empty: Stream[A])((x,y) => if (p(x)) Stream.cons(x,y) else y)
      
    def append [B >: A] (a: Stream[B]): Stream[B] = 
      foldRight(a)((x,y) => Stream.cons(x,y))
      
    def flatMap[B] (f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty: Stream[B])((x,y) => f(x).append(y))
      
    def map1[B](f: A => B): Stream[B] =
      unfold(uncons)
      { case Some((x,s)) => Some(f(x), s.uncons)
        case None => None }
    
    def take1(n: Int): Stream[A] = 
      unfold((uncons,n))
      {case (_,0) => None
       case (None,_) => None
       case (Some((x,s)), i) => Some(x, (s.uncons, i - 1)) 
      }
    
    def takeWhile2(p: A => Boolean): Stream[A] =
      unfold(uncons)
      { case Some((x,s)) => if (p(x)) Some(x, s.uncons) else None
        case None => None 
      }
    
  }
  
  object Stream {
    
    def empty[A]: Stream[A] =
      new Stream[A]{ def uncons = None}
    
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
      lazy val uncons = Some((hd,tl))
    }
    
    def apply[A](as: A*): Stream[A] =
      if(as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }
  
  val ones: Stream[Int]  = Stream.cons(1, ones)
  
  printf(ones.exists(_ % 2 != 0).toString)
  printf(ones.map(_ + 1).exists(_ % 2 == 0).toString)
  printf(ones.forall(_ != 1).toString)
  printf(ones.takeWhile1(_ == 1).take(100).toList.toString)
  printf("\n")
  
  def constant(a: Int): Stream[Int] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def fibAcc(a: Int, b : Int): Stream[Int] = Stream.cons(a, fibAcc(b, a+b))
   fibAcc(0,1) 
  }
  printf(fibs.take(100).toList.toString)
  printf("\n")
  
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((x,y)) => Stream.cons(x, unfold(y)(f))
  }
  
  def constant1(a: Int): Stream[Int] = unfold(true)(x => Some((a,x)))
  def from1(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))
  def fibs1: Stream[Int] = unfold((0,1)){case (x,y) => Some(x, (y, x+y))}
  val ones1: Stream[Int] = unfold(true)(x => Some((1,x)))
  
  printf(ones1.takeWhile1(_ == 1).take(100).toList.toString)
  printf("\n")
  
  printf(fibs1.take(100).toList.toString)
  printf("\n")
  
  
  
  
  
}