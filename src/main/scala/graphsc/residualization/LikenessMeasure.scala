package graphsc
package residualization

class LikenessCalculator(total: Boolean = false) {
  
  def restrict(ren: Renaming)(res: Option[(Int, Renaming)]): Option[(Int, Renaming)] =
    for((i,rn) <- res; newren <- ren | rn) yield (i,newren)
  
  def combine(l: List[Int]): Int = 
    (Int.MaxValue :: l).min match {
      case Int.MaxValue => Int.MaxValue
      case n => n + 1
    }
  
  def viablePermutations(r: RenamedNode): List[Renaming] =
    viablePermutations(r.node).map(p => r.renaming comp p comp r.renaming.inv)
  
  // tries to guess under what permutations the node may be invariant 
  def viablePermutations(n: Node): List[Renaming] = {
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
  
  def likeness(
        l: RenamedNode, r: RenamedNode, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] = restrict(ren) {
    likenessN(l.node, r.node, l.renaming comp ren comp r.renaming.inv, hist)
      .map{ case (i, newren) => (i, l.renaming.inv comp newren comp r.renaming) }
  }
  
  def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] = restrict(ren) {
    val ldef = definingHyperedgesNonStrict(l)
    val rdef = definingHyperedgesNonStrict(r)
    
    lazy val idren = Renaming(r.used) | ren
    
    lazy val goodids = 
      l.outs.toList.collect{ case h if h.label == Id() => 
         ((h.source.renaming.inv comp h.dests(0).renaming) | ren)}.flatten
 
    if(l == r && idren.nonEmpty) 
      Some((Int.MaxValue, idren.get))  
    else if(l == r && goodids.nonEmpty)
      // a node may be equivalent to itself up to non-id renaming
      Some((Int.MaxValue, goodids(0)))
    else if(ldef.isEmpty || rdef.isEmpty || hist.exists(p => p._1 == l || p._2 == r))
      Some((0, ren))
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
        // final hyperedges always contradict to caseofs when non-total 
        if(!total && ((lfinals.nonEmpty && rdef.nonEmpty) || (rfinals.nonEmpty && ldef.nonEmpty)))
          None
        else {
          val ress =
            for(lh <- ldef; rh <- rdef) yield {
              val l = likenessH(lh, rh, ren, hist)
              // if scrutinee variables are the same and the caseofs are incompatible then
              // the nodes cannot be equal
              if(l == None &&
                 lh.label.isInstanceOf[CaseOf] && rh.label.isInstanceOf[CaseOf] && 
                 (lh.source.renaming.inv comp lh.dests(0)).getVar == 
                   (ren comp rh.source.renaming.inv comp rh.dests(0)).getVar)
                return None
              else l match {
                case None => (0, ren)
                case Some(x) => x
              }
            }
          Some(((0, ren) /: ress)((l,r) => (l._1 max r._1, l._2 & r._2)))
        }
      }
    }
  }
  
  def likenessH(
        lh1: Hyperedge, rh1: Hyperedge, ren1: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] = restrict(ren1) {
    val ln = lh1.source.node
    val rn = rh1.source.node
    val ren = lh1.source.renaming comp ren1 comp rh1.source.renaming.inv
    
    val res =
      if(lh1.label != rh1.label || lh1.dests.size != rh1.dests.size)
        None
      else {
        lh1.label match {
          case Var() => 
            if(ren(0) == 0) Some((Int.MaxValue, ren))
            else if(ren(0) == -1) (ren | Renaming(0 -> 0)).map((Int.MaxValue, _)) 
            else None
          case _ =>
            val ldests = lh1.dests
            val rdests = rh1.dests
            
            var newren: Option[Renaming] = Some(ren)
            
            val scores =
              for(((ld,rd),sh) <- ldests zip rdests zip lh1.shifts) yield {
                val score =
                  if(sh == -1) {
                    likenessN(ld.node, rd.node, 
                        ld.renaming.inv comp rd.renaming, (ln,rn) :: hist)
                  } else {
                    (newren.get.shift(sh) | Renaming(0 until sh toSet))
                      .flatMap(likeness(ld, rd, _, (ln,rn) :: hist))
                  } 
                
                score match {
                  case None => return None
                  case Some((sc,rn)) =>
                    if(sh != -1) {
                      newren = newren | rn.unshift(sh)
                      if(newren.isEmpty) return None
                    }
                    sc
                }
              }
            
            Some((combine(scores), newren.get))
        }
      }
    
    res.map{ case (i, newren) => 
      (i, lh1.source.renaming.inv comp newren comp rh1.source.renaming) }
  }
}

class NonrenamingLikenessCalculator(total: Boolean) extends LikenessCalculator(total) {
  val lk = new LikenessCalculator(total)
  
  override def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] =
    lk.likenessN(l, r, ren, hist) match {
      case None => None
      case Some((v, _)) => Some((v, ren))
    }
}
