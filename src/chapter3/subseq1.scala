package chapter3

object subseq1 extends App{
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def subseqAcc(listleft: List[A], subleft: List[A]): Boolean = (listleft,subleft) match {
      case (_,Nil) => true
      case (Nil,_) => false
      case (x::xs, y::ys) => if(x == y) subseqAcc(xs,ys) else subseqAcc(xs,sub)
    }
    subseqAcc(l, sub)
  }

printf(hasSubsequence(List(1,2,3,4),List(2,3)).toString)
    printf(hasSubsequence(List(1,2,3,4),List(1,3)).toString)  
}