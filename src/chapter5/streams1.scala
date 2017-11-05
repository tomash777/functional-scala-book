package chapter5

object streams1 extends App{
  
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] = uncons match {
      case None => Nil
      case Some((hd,tl)) => hd :: tl.toList
    }
    def take(n: Int): Stream[A] = (uncons,n) match {
      case (None,_) => Stream.empty
      case (_,0) => Stream.empty
      case (Some((hd,tl)),i) => Stream.cons(hd, tl.take(i-1))
    }
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case None => Stream.empty
      case Some((hd,tl)) => if(p(hd)) Stream.cons(hd, tl.takeWhile(p)) else Stream.empty
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
      }
    def exists(p: A => Boolean): Boolean = foldRight(false)(p(_)|| _)
    def forall(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)
    
    def takeWhile1 (p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty: Stream[A])((h,t) => if(p(h)) Stream.cons(h,t) else Stream.empty)
      
    def map[B](f: A => B): Stream[B] = 
      foldRight(Stream.empty: Stream[B])((h,t) => Stream.cons(f(h),t))
      
    def filter (p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty: Stream[A])((h,t) => if(p(h)) Stream.cons(h,t) else t)
      
    def append[B >: A] (b: Stream[B]): Stream[B] = foldRight(b)((h,t) => Stream.cons(h,t))
    
    def flatMap[B] (f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty: Stream[B])((x,y) => f(x).append(y))
    
    def map1[B] (f: A => B): Stream[B] = 
      unfold(uncons)(_ map { case (a,s) => (f(a), s.uncons)})
    
    def take1 (n: Int): Stream[A] = 
      unfold((n,uncons))
          {case (0,_) => None 
           case (_,None) => None
           case (i, Some((a,s))) => Some((a,(i-1, s.uncons))) 
          }
    /*
    def tails: Stream[Stream[A]] = unfold(uncons){ case None => 
    */
  }

  object Stream {
    def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }
    
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
    lazy val uncons = Some((hd, tl))
    }
    
    def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
  
  val ones: Stream[Int]  = Stream.cons(1, ones)
  def constant[A] (a: A): Stream[A] = Stream.cons(a,constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def fibAcc(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibAcc(b, a+b))
    fibAcc(0,1)
  }
  
  printf(ones.exists(_ % 2 != 0).toString)
  printf(ones.map(_ + 1).exists(_ % 2 == 0).toString)
  printf(ones.forall(_ != 1).toString)
  printf(ones.takeWhile1(_ == 1).take(100).toList.toString)
  printf("\n")
    printf(fibs.take(100).toList.toString)
  printf("\n")
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((a,s)) => Stream.cons(a, unfold(s)(f))
  }
  
  val ones1: Stream[Int] = unfold(1)(x => Some(1,x))
  
  printf(ones1.takeWhile1(_ == 1).take(100).toList.toString)
  printf("\n")
  
  def from1(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))
  def fibs1: Stream[Int] = unfold((0,1)){case (a,b) => Some((a, (b,a+b)))}
  
  printf(fibs1.take(100).toList.toString)
  printf("\n")
  
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = (s.uncons,s2.uncons) match {
    case (_,None) => true
    case (None,_) => false
    case (Some((h,t)),Some((h2,t2))) => h == h2 && startsWith(t,t2)
  }
  
printf(startsWith(Stream(1,2,3), Stream(1,2)).toString + "\n")  
printf(startsWith(Stream(1,2,3), Stream(1,2,3)).toString + "\n")  
printf(startsWith(Stream(1,2), Stream(1,2,3)).toString + "\n")  
printf(startsWith(Stream(1,2,3,4,5), Stream(1,2,4,5)).toString + "\n")  
printf(startsWith(ones1, Stream(1,2)).toString + "\n")  
printf(startsWith(ones1, Stream(1,1,1,1)).toString + "\n")  
printf(startsWith(ones1, fibs1).toString + "\n")  
printf(startsWith(Stream(1,2,3), Stream(1,2)).toString + "\n")  
  

  
  
}