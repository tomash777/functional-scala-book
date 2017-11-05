package chapter4

object matching extends App{
  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] = 
    try { Some(Pattern.compile(s)) } catch {case e: PatternSyntaxException => None}
    
  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => s => p.matcher(s).matches)
  
  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {p <- pattern(pat)} yield ((s: String) => p.matcher(s).matches) 
      
  def doesMatch(pat: String, s:String): Option[Boolean] =
    for {p <- mkMatcher_1(pat)} yield p(s)
    
  def bothMatch(pat: String, pat1: String, s: String): Option[Boolean] =
    for {p1 <- mkMatcher_1(pat); p2 <- mkMatcher_1(pat1)} yield p1(s) && p2(s)
      
  def bothMatch_1(pat: String, pat1: String, s: String): Option[Boolean] = 
    mkMatcher(pat).flatMap(f => (mkMatcher(pat1) map (g => f(s) && g(s))))
    
  def map2[A,B,C] (a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (x => (b map (y => f(x,y))))
    
  def bothMatch_2(pat: String, pat1: String, s: String): Option[Boolean] =
    map2[String => Boolean, String => Boolean, String => Boolean](mkMatcher(pat), mkMatcher(pat1))((f,g) => (x => f(x) && g(x))) map (f => f(s))
  
  def foldRight[A,B] (l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x::xs => f(x, foldRight(xs,z)(f))
  }
    
  def sequence[A] (a: List[Option[A]]): Option[List[A]] =
    foldRight(a, Some(Nil): Option[List[A]])(map2(_,_)(_::_))
    
  def traverse[A,B] (a: List[A])(f: A => Option[B]): Option[List[B]] = 
    foldRight(a, Some(Nil): Option[List[B]])((x,l) => map2(f(x),l)(_::_))
    
  def sequence1[A] (a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x) 
    
    printf(sequence(List(Some(2),Some(3),Some(4))).toString)
    printf(sequence(List(Some(2),None,Some(4))).toString)
    printf(sequence1(List(Some(2),Some(3),Some(4))).toString)
    printf(sequence1(List(Some(2),None,Some(4))).toString)

}