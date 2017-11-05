package chapter3

object fold1 extends App{
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x,foldRight(xs,z)(f))
  }
  
  printf( foldRight( List(1,2,3), Nil: List[Int])(_::_).toString)
  
  def length[A] (l: List[A]): Int = foldRight(l,0)((x,y)=>y+1)
  
  def foldLeft[A,B] (l:List[A], z:B)(f: (B,A) => B): B = {
    def foldAcc(left: List[A], acc: B): B = left match {
      case Nil => acc
      case x::xs => foldAcc(xs, f(acc,x))
    }
    foldAcc(l,z)
  }
  
  def lengthLeft[A] (l: List[A]): Int = foldLeft(l,0)((x,y)=>x+1)
  
  def sumLeft (l: List[Double]): Double = foldLeft(l,0.0)((x,y)=>x+y)
  
  def productLeft (l:List[Double]): Double = foldLeft(l,1.0)((x,y) => x*y)
  
  def reverse[A] (l: List[A]): List[A] = foldLeft (l, Nil: List[A])((l1,x)=> x::l1)
  
    printf( reverse(List(1,2,3)).toString)
    
  def append[A] (l1: List[A], l2: List[A]) = foldRight(l1,l2)((x,l) => x::l)
  
  def concatenate[A] (l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  printf(concatenate(List(List(1,3),List(2,4))).toString)

  def foldLeft1[A,B] (l:List[A], z:B)(f: (B,A) => B): B = foldRight(reverse(l),z)((x,y)=>f(y,x))
  
    def productLeft1 (l:List[Double]): Double = foldLeft1(l,1.0)((x,y) => x*y)
    
    printf((productLeft1(List(2.0,3.0,4.0))).toString)
  
  def foldRight1[A,B] (l:List[A], z:B)(f: (A,B) => B): B = foldLeft(reverse(l),z)((x,y)=>f(y,x))
  
  def concatenate1[A] (l: List[List[A]]): List[A] = foldRight1(l, Nil: List[A])(append)

  printf(concatenate1(List(List(1,3),List(2,4))).toString)
  
  def scanLeft[A,B] (l: List[A], z: B)( f:(B,A) => B): List[B] = {
    def scanLeftAcc (listleft: List[A], acc: List[B]): List[B] = listleft match {
      case Nil => acc
      case x::xs => scanLeftAcc(xs, f(acc.head,x)::acc)
    }
    reverse(scanLeftAcc(l,List(z)))
  }
  
  def scanRight[A,B] (l: List[A], z: B)(f: (A,B) => B): List[B] = l match {
    case Nil => List(z)
    case x::xs => f(x,scanRight(xs,z)(f).head)::scanRight(xs,z)(f)
  }
}