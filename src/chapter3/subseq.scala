package chapter3

object subseq extends App{
  
  def hasSubsequence[A] (l: List[A], sub: List[A]): Boolean = {
    def seqAcc (l1: List[A], sub1: List[A]): Boolean = (l1,sub1) match {
      case (_,Nil) => true
      case (Nil, _) => false
      case (x::xs, y::ys) => if (x == y) seqAcc(xs, ys) else seqAcc(xs, sub)
    }
    seqAcc (l,sub)
  }
    
  printf(hasSubsequence(List(1,2,3,4),List(2,3)).toString)
    printf(hasSubsequence(List(1,2,3,4),List(1,3)).toString)

  
  
}