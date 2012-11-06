package graphsc
package residualization

trait LikenessMeasure[L] {
  def zero: L
  def infinity: L
  def combine(l: List[L]): L
}

object LikenessMeasure {
  implicit object IntLikenessMeasure extends LikenessMeasure[Int] {
    override def zero = 0
    override def infinity = Int.MaxValue
    override def combine(l: List[Int]) = 
      (infinity :: l).min match {
        case Int.MaxValue => Int.MaxValue
        case n => n + 1
      }
  }
}

case class LikenessCalculator[L](implicit lm: LikenessMeasure[L], ord: Ordering[L]) {
  import lm._
  
  implicit def injectOr(r: Option[Renaming]) = new {
    def |(l: Renaming): Option[Renaming] = r.flatMap(_ | l)
    def |(l: Option[Renaming]): Option[Renaming] = 
      for(x <- r; y <- l; k <- x | y) yield k 
  }
  
  def likeness(
        l: RenamedNode, r: RenamedNode,
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = {
    likenessN(l.node, r.node, hist).map {
      case (i,ren) => 
        (i, l.renaming comp ren comp r.renaming.inv)
    }
  }
  
  def likenessN(
        l: Node, r: Node,
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = {
    val ldef = definingHyperedgesNonStrict(l)
    val rdef = definingHyperedgesNonStrict(r)
 
    if(l.used.size != r.used.size)
      // TODO: Sometimes there may be unused variables that are hard to get rid of
      None
    else if(l == r)
      Some((infinity, Renaming(r.used)))
    else if(ldef.isEmpty || rdef.isEmpty || hist.exists(p => p._1 == l || p._2 == r))
      Some((zero, Renaming()))
    else {
      val ress =
        for(lh <- ldef; rh <- rdef) yield
          likenessH(lh, rh, hist)
      ress.filter(_.nonEmpty) match {
        case Nil => None
        case l => l.maxBy(_.get._1)
      }
    }
  }  
  
  def likenessH(
        lh1: Hyperedge, rh1: Hyperedge,
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = {
    val ln = lh1.source.node
    val rn = rh1.source.node
    
    if(lh1.label != rh1.label || lh1.dests.size != rh1.dests.size)
      None
    else {
      lh1.label match {
        case Var() =>
          Some((infinity, lh1.source.renaming comp rh1.source.renaming.inv))
        case _ =>
          val ldests = lh1.source.renaming.inv.compDests(lh1)
          val rdests = rh1.source.renaming.inv.compDests(rh1)
          
          if(lh1.label == Let()) {
            // If it's a let, we need to rearrange arguments
            likeness(ldests(0), rdests(0), (ln,rn) :: hist).flatMap {
              case (head_score, headren) =>
                val ltail = ldests.tail
                val rtail = rdests.tail
                
                val chld =
                  for((i,j) <- headren.vector.zipWithIndex if i != -1) yield
                    likeness(ltail(i), rtail(j), (ln,rn) :: hist)
                
                if(!chld.forall(_.isDefined))
                  None
                else {
                  val rens = chld.map(_.get._2)
                  val resren =
                      (Some(Renaming()).asInstanceOf[Option[Renaming]] /: rens)(_ | _)
                  resren.map {
                    rr =>
                      // if our head renaming doesn't cover all used variables
                      // then we cannot return the full score
                      if(chld.size < (ldests(0).used.size max rdests(0).used.size))
                        (combine(lm.zero :: head_score :: chld.map(_.get._1)), rr)
                      else
                        (combine(head_score :: chld.map(_.get._1)), rr)
                  }
                }
            }
          }
          else { // if it's not a let
            val chld = (ldests,rdests).zipped.map(likeness(_, _, (ln,rn) :: hist))
            
            if(!chld.forall(_.isDefined))
              None
            else {
              // here we have to unshift our renamings
              
              val shifts = lh1.shifts
              // these renamings make sure that bound varibales match
              val shift_rens = shifts.map(n => Renaming(0 until n toSet))
              
              val rens = 
                (chld.map(_.get._2), shift_rens, shifts).zipped.map(
                    (a,b,n) => 
                      if(n != -1) (a | b).map(_.unshift(n))
                      else (a | b))
                    
              val resren = 
                (Some(Renaming()).asInstanceOf[Option[Renaming]] /: rens)(_ | _)
                
              resren.map((combine(chld.map(_.get._1)), _))
            }
          }
      } 
    }
  }
}