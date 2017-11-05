package chapter4

object options extends App{
    trait Option[+A]{
    case class Some[+A] (get: A) extends Option[A]
    case object None extends Option[Nothing]
    
    def map[B] (f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
    }
    def flatMap[B] (f: A => Option[B]): Option[B] = this match {
      case Some(x) => f(x)
      case None => None
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) => if (f(x)) this else None
      case None => None
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match{
      case Some(x) => this
      case None => ob
    }
    def mean(xs: Seq[Double]): Option[Double] = 
      if(xs.isEmpty) None
      else Some(xs.sum/xs.length)
  }
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}