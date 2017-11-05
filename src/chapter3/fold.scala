package chapter3

object fold extends App{
  
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
  
  printf( foldRight( List(1,2,3), Nil: List[Int])(_::_).toString)
  
  def length[A] (l: List[A]): Int = foldRight(l,0)((x,y) => y+1)
printf( length(List(1,2,3)).toString)

  def foldLeft[A,B] (l: List[A], z: B)(f:(B,A) => B): B = {
    
    def foldAcc(l1: List[A], t:B): B = l1 match {
      case Nil => t
      case x::xs => foldAcc(xs,f(t,x))
    }
      
    foldAcc(l,z)
  }
  
  def lengthLeft[A] (l: List[A]): Int = foldLeft(l,0)((x,y) => x+1)
    

  printf( lengthLeft(List(1,2,3)).toString)
  
  def reverse[A] (l: List[A]): List[A] = foldRight(l, Nil: List[A])((x,l1) => l1++List(x))

  printf(reverse(List(1,2,3)).toString)
  
  def append[A] (l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(_::_)
  
  def concatenate[A] (l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(_++_)
  
  printf(concatenate(List(List(1,3),List(2,4))).toString)
  
  def foldLeft1[A,B] (l: List[A], z: B)(f:(B,A) => B): B = 
    foldRight[A,B] (reverse(l), z) ((x,y) => f(y,x))
  
    def concatenate1[A] (l: List[List[A]]): List[A] = foldLeft1(l, Nil: List[A])(_++_)
  
  printf(concatenate1(List(List(1,3),List(2,4))).toString)
  
  def scanRight[A,B] (l: List[A], z: B)(f: (A,B) => B): List[B] = l match {
    case Nil => List(z)
    case x::xs => f(x, scanRight(xs, z)(f).head) :: scanRight(xs, z)(f)
  }
  
  def scanLeft[A,B] (l: List[A], z: B)(f: (B,A) => B) : List[B] = {
     def scanAcc (l1: List[A], l2: List[B]): List[B] = l1 match {
       case Nil => l2
       case x::xs => scanAcc(xs, f(l2.head, x)::l2)
     }
    
     scanAcc(l,List(z))
  }
  
}

