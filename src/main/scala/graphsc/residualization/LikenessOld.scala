package graphsc
package residualization


class OldLikenessCalculator(total: Boolean = false) extends LikenessCalculator(total) {

  type L = Int
  def zero = 0
  def infinity = Int.MaxValue
  
  implicit def injectOr(r: Option[Renaming]) = new {
    def |(l: Renaming): Option[Renaming] = r.flatMap(_ | l)
    def |(l: Option[Renaming]): Option[Renaming] = 
      for(x <- r; y <- l; k <- x | y) yield k 
  }
  
  override def viablePermutations(r: RenamedNode): List[Renaming] =
    viablePermutations(r.node).map(p => r.renaming comp p comp r.renaming.inv)
  
  // tries to guess under what permutations the node may be invariant 
  override def viablePermutations(n: Node): List[Renaming] = {
    if(total && n.deref.getVarUnused.isEmpty && n.used.size > 1) {
      val df = definingHyperedgesNonStrict(n)
      val constructs = df.filter(_.label.isInstanceOf[Construct])
      val caseofs = df.filter(_.label.isInstanceOf[CaseOf]) 
      val casevars = 
        (for(h <- caseofs;  v <- (h.source.renaming.inv comp h.dests(0)).getVar) 
          yield v).distinct
      val consrens =
        constructs.flatMap{ h =>
          val lst = h.dests.map(viablePermutations(_))
          (lst.head /: lst.tail)(_ ++ _).toList.distinct
            .map(p => h.source.renaming.inv comp p comp h.source.renaming)
        }
      val caserens =
        for(perm <- casevars.permutations; 
            map = casevars zip perm; 
            if map.exists(p => p._1 != p._2))
          yield Renaming(map:_*)
      ( consrens ++ caserens).distinct
    } else
      Nil
  }
  
  override def likeness(
        l: RenamedNode, r: RenamedNode, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = {
    likenessN(l.node, r.node, ren, hist).map {
      case (i,ren) => 
        (i, l.renaming comp ren comp r.renaming.inv)
    }
  }
  
  override def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = {
    val ldef = definingHyperedgesNonStrict(l)
    val rdef = definingHyperedgesNonStrict(r)
 
    if(l == r)
      // in total setting a node may be equivalent to itself up to non-id renaming
      Some((infinity, if(total) Renaming() else Renaming(r.used)))
    else if(ldef.isEmpty || rdef.isEmpty || hist.exists(p => p._1 == l || p._2 == r))
      Some((zero, Renaming()))
    else {
      val lfinals = ldef.filter(h => !h.label.isInstanceOf[CaseOf])
      val rfinals = rdef.filter(h => !h.label.isInstanceOf[CaseOf])
      
      // there cannot be more than one non-caseof hyperedge even in total setting
      assert(lfinals.length <= 1 && rfinals.length <= 1)
      
      val fin =
        for(lh <- lfinals; rh <- rfinals) yield 
          likenessH(lh, rh, ren, hist) 
      
      if(fin.nonEmpty)
        fin(0)
      else {
        val ress =
          for(lh <- ldef; rh <- rdef) yield
            likenessH(lh, rh, ren, hist)
        
        if(total) {
          // when totality holds, little inconsistencies don't mean anything
          ((None.asInstanceOf[Option[(L, Renaming)]] /: ress) {
            case (None,b) => b
            case (a,None) => a
            case (Some((v1, ren1)), Some((v2, ren2))) =>
              Some((v1 max v2, ren1 & ren2))
          }).map{case (v,_) => (v,Renaming())}
        } else {
          ress.filter(_.nonEmpty) match {
              case Nil =>
                None
              case l => l.maxBy(_.get._1)
          }
        }
      }
    }
  }  
  
  override def likenessH(
        lh1: Hyperedge, rh1: Hyperedge, ren: Renaming = Renaming(),
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
            likeness(ldests(0), rdests(0), ren, (ln,rn) :: hist).flatMap {
              case (head_score, headren) =>
                val ltail = ldests.tail
                val rtail = rdests.tail
                
                val chld =
                  for((i,j) <- headren.vector.zipWithIndex if i != -1) yield
                    likeness(ltail(i), rtail(j), ren, (ln,rn) :: hist)
                
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
                        (combine(zero :: head_score :: chld.map(_.get._1)), rr)
                      else
                        (combine(head_score :: chld.map(_.get._1)), rr)
                  }
                }
            }
          }
          else { // if it's not a let
            val chld = (ldests,rdests).zipped.map(likeness(_, _, ren, (ln,rn) :: hist))
            
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
