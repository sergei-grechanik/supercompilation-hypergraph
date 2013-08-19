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
  
  override def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(L, Renaming)] = restrict(ren) {
    val ldef = definingHyperedgesNonStrict(l)
    val rdef = definingHyperedgesNonStrict(r)
    
    if(l == r)
      // in total setting a node may be equivalent to itself up to non-id renaming
      Some((infinity, Renaming(r.used)))
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
          }).fold(Some(0,ren)){case (v,_) => Some((v,Renaming()))}
        } else {
          ress.filter(_.nonEmpty) match {
              case Nil =>
                Some((0, ren))
              case l => l.maxBy(_.get._1)
          }
        }
      }
    }
  }  
}
