package graphsc
package residualization

case class EquivalenceProver[S, L]
    (issue_hyperedge: Hyperedge => Unit = _ => ())
    (implicit cc: CorrectnessChecker[S], lm: LikenessMeasure[L], ord: Ordering[L]) {
  val likenesscalc = LikenessCalculator[L]()
  import lm._
  import likenesscalc._
  
  def prove(
      l: Node, 
      r: Node, 
      ren: Renaming = Renaming(), 
      hist: List[((Node, S), (Node, S))] = Nil): Option[Renaming] = {
    val lind = hist.indexWhere(_._1._1 == l)
    val rind = hist.indexWhere(_._2._1 == r)

    if(l == r)
      ren | Renaming(r.used)
    else if(lind != -1 && rind != -1) {
      hist.find(p => p._1._1 == l && p._2._1 == r) match {
        case Some(((_, lsafety), (_, rsafety))) if cc.safe(lsafety) && cc.safe(rsafety) =>
          // TODO: I think we should add some variables to ren (if they guarantee correctness)
          Some(ren)
        case _ => None
      }
    } else {
      val louts = l.outs.groupBy(_.label)
      val routs = r.outs.groupBy(_.label)
      
      val pairs = 
        for((llab,lset) <- louts.iterator; (rlab,rset) <- routs.iterator; if llab == rlab; 
            lh <- lset; rh <- rset; if lh.dests.size == rh.dests.size) yield {
          likenessH(lh, rh).flatMap {
            case (like, ren1) => (ren | ren1).map((like, lh, rh, _))
          }
        }
      
      val sorted_pairs = 
        pairs.collect{ case Some(p) => p }.toList.sortBy(_._1)
      
      var result: Option[Renaming] = None
        
      for((like, lh, rh, ren1) <- sorted_pairs; if result == None) {
        val newhisthead = ((l, cc(l.deref)), (r, cc(r.deref)))
        val newss1 =
          (newhisthead :: hist).map(p => cc.through(p._1._2, lh)).transpose
        val newss2 = 
          (newhisthead :: hist).map(p => cc.through(p._2._2, rh)).transpose
        
        var curren: Option[Renaming] = 
          Some(lh.source.renaming comp ren1 comp rh.source.renaming.inv)
        
        for((((ld, rd), sh), (news1, news2)) <- 
            ((lh.dests zip rh.dests) zip lh.shifts zip (newss1 zip newss2))) {
          if(curren != None) {
            val newhist =
              ((newhisthead :: hist) zip (news1 zip news2)).map {
                case (((n1,_), (n2,_)), (s1, s2)) => 
                  ((n1, s1), (n2, s2)) 
              }
            val shiftedren = curren.get.shift(sh) | Renaming(0 until sh toSet)
            curren = curren |
              shiftedren.flatMap(shren =>
                prove(ld.node, rd.node, 
                      ld.renaming.inv comp shren comp rd.renaming, 
                      newhist).map(r => (ld.renaming comp r comp rd.renaming.inv).unshift(sh)))
          }
        }
        
        result = curren.map(lh.source.renaming.inv comp _ comp rh.source.renaming)
      }
        
      if(hist.isEmpty && result != None)
        issue_hyperedge(Hyperedge(Id(), result.get comp r, List(l.deref)))
        
      result
    }
  }
}