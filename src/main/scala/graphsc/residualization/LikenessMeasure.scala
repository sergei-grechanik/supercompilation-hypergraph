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
  
  def definingHyperedge(n: Node): List[Hyperedge] = {
    val hypers = 
      n.outs.filter(h => h.label match {
        case Construct(_) => true
        case Tick() => true
        case Var() => true
        case Error() => true
        case CaseOf(_) if h.dests(0).getVar.isDefined => true 
        case _ => false
      })
      
    // only caseofs can be multiple (due to unpropagated information)
    assert(hypers.size <= 1 || hypers.head.label.isInstanceOf[CaseOf])
    hypers.toList
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
    val ldef = definingHyperedge(l)
    val rdef = definingHyperedge(r)
 
    if(l == r)
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
          val lh = lh1.source.renaming.inv.compDests(lh1)
          val rh = rh1.source.renaming.inv.compDests(rh1)
          
          if(lh.label == Let()) {
            // If it's a let, we need to rearrange arguments
            likeness(lh.dests(0), rh.dests(0), (ln,rn) :: hist).flatMap {
              case (head_score, headren) =>
                val ltail = lh.dests.tail
                val rtail = rh.dests.tail
                
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
                      if(chld.size < (lh.dests(0).used.size max rh.dests(0).used.size))
                        (combine(lm.zero :: head_score :: chld.map(_.get._1)), rr)
                      else
                        (combine(head_score :: chld.map(_.get._1)), rr)
                  }
                }
            }
          }
          else { // if it's not a let
            val chld = (lh.dests,rh.dests).zipped.map(likeness(_, _, (ln,rn) :: hist))
            
            if(!chld.forall(_.isDefined))
              None
            else {
              // here we have to unshift our renamings
              
              val shifts = lh.shifts
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