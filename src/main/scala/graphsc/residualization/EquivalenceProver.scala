package graphsc
package residualization

class EquivalenceProver[S, L](scc: SCC = null)
    (implicit cc: CorrectnessChecker[S], lm: LikenessMeasure[L], ord: Ordering[L]) {
  val likenesscalc = LikenessCalculator[L]()
  import lm._
  import likenesscalc._
  
  type Hist = List[((Node, S), (Node, S))]
  
  val cache = collection.mutable.Map[(Node, Node, Renaming, Hist), Option[EqProofTree]]()
  
  var stats = collection.mutable.Map[(Node, Node), Int]()
  
  def prove(
      l1: Node, 
      r1: Node, 
      ren1: Renaming = Renaming(), 
      hist2: Hist = Nil): Option[EqProofTree] = {
    
    val hist1 = 
      if(scc != null) {
        val lcompid = scc.componentOf(l1)
        val rcompid = scc.componentOf(r1) 
        hist2.takeWhile(p => 
          scc.componentOf(p._1._1) == lcompid && scc.componentOf(p._2._1) == rcompid) 
      } else {
        hist2
      }
    
    val swap = l1.hashCode > r1.hashCode
    
    val args@(l, r, ren, hist) =
      if(!swap) (l1, r1, ren1, hist1)
      else {
        val (lh,rh) = hist1.unzip
        (r1, l1, ren1.inv, rh zip lh)
      }
    
    cache.getOrElseUpdate(args, {
      val res = proveUncached(l, r, ren, hist)
      
      if(res == None && likeness(l.deref,r.deref) != None)
        if(stats.contains((l,r)))
          stats((l,r)) += 1
        else
          stats += (l,r) -> 1
        
      res
    })
  }
    
  def proveUncached(
      l: Node, 
      r: Node, 
      ren: Renaming = Renaming(), 
      hist: Hist = Nil): Option[EqProofTree] = {
    val lind = hist.indexWhere(_._1._1 == l)
    val rind = hist.indexWhere(_._2._1 == r)

    if(l == r)
      (ren | Renaming(r.used)).map(EqProofTree(_, (l,r)))
    else if(lind != -1 && rind != -1) {
      hist.find(p => p._1._1 == l && p._2._1 == r) match {
        case Some(((_, lsafety), (_, rsafety))) if cc.safe(lsafety) && cc.safe(rsafety) =>
          // TODO: I think we should add some variables to ren (if they guarantee correctness)
          Some(EqProofTree(ren, (l,r)))
        case _ => None
      }
    } else if(lind != -1 || rind != -1) {
      // Well, we trade precision for efficiency here
      None
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
        pairs.collect{ case Some(p) => p }.toList.distinct.sortBy(_._1)(Ordering[L].reverse)
      
      var result: Option[Renaming] = None
      var hypers: (Hyperedge, Hyperedge) = null
      var subtrees: List[EqProofTree] = null
        
      for((like, lh, rh, ren1) <- sorted_pairs; if result == None) {
        val newhisthead = ((l, cc(l.deref)), (r, cc(r.deref)))
        val newss1 =
          (newhisthead :: hist).map(p => cc.through(p._1._2, lh)).transpose
        val newss2 = 
          (newhisthead :: hist).map(p => cc.through(p._2._2, rh)).transpose
        
        var curren: Option[Renaming] = 
          Some(lh.source.renaming comp ren1 comp rh.source.renaming.inv)
        
        subtrees = Nil
        
        for((((ld, rd), sh), (news1, news2)) <- 
            ((lh.dests zip rh.dests) zip lh.shifts zip (newss1 zip newss2))) {
          if(curren != None) {
            val newhist =
              ((newhisthead :: hist) zip (news1 zip news2)).map {
                case (((n1,_), (n2,_)), (s1, s2)) => 
                  ((n1, s1), (n2, s2)) 
              }
            
            if(sh != -1) {
              // it is a normal or shifted child
              val shiftedren = curren.get.shift(sh) | Renaming(0 until sh toSet)
              
              val subtree =
                shiftedren.flatMap(shren =>
                  prove(ld.node, rd.node, 
                        ld.renaming.inv comp shren comp rd.renaming, 
                        newhist))
              
              subtree.foreach { t => subtrees = t :: subtrees }     
                      
              curren = curren |
                subtree.map(t => (ld.renaming comp t.renaming comp rd.renaming.inv).unshift(sh))
            }
            else {
              // this is the zeroth dest of a let, it has independent renaming
              // TODO: Actually the may be equal up to some non-id renaming, so
              // we might want to rearrange other dests accordingly (seems not easy though)
              val subtree = prove(ld.node, rd.node, Renaming(ld.used | rd.used), newhist)
              subtree match {
                case Some(t) => subtrees = t :: subtrees
                case None => curren = None
              }
            }
          }
        }
        
        result = curren.map(lh.source.renaming.inv comp _ comp rh.source.renaming)
        hypers = (lh, rh)
      }
        
      result.map(resren => 
        EqProofTree(resren, (l,r), Some((hypers._1, hypers._2, subtrees.reverse))))
    }
  }
}

case class EqProofTree(
    renaming: Renaming, 
    nodes: (Node, Node), 
    out: Option[(Hyperedge, Hyperedge, List[EqProofTree])] = None) {
  
  // Why can we glue child nodes too, not only roots?
  // Because they are defined in terms of themselves and the roots.
  def performGluing(g: Hypergraph) {
    for((_,_,l) <- out; t <- l)
      t.performGluing(g)
    if(nodes._1.deref.node != nodes._2.deref.node)
      g.add(Id(), renaming comp nodes._2, List(nodes._1.deref))
  }
  
  def toDot: String = {
    val label =
      nodes._1 + "\\l" + renaming + "\\l" + nodes._2 + "\\l\\l" +
      nodes._1.prettyDebug.replace("\n", "\\l") + "\\l\\l" +
      nodes._2.prettyDebug.replace("\n", "\\l") + "\\l"
    "\"" + nodes.toString + "\"" + "[label=\"" + label + "\",shape=box];\n" +
    (out match {
      case None => ""
      case Some((h1, h2, subs)) =>
        "\"" + (h1,h2).toString + "\"" + "[label=\"{" + h1.label + "|{" + 
          subs.indices.map("<" + _ + ">").mkString("|") + "}}\",shape=record];" +
        subs.zipWithIndex.map{ case (t,i) =>
            "\"" + (h1,h2).toString + "\":" + i + " -> " + "\"" + t.nodes.toString() + "\";\n"
          }.mkString("\n") +
        "\"" + nodes.toString + "\"" + " -> " + "\"" + (h1,h2).toString + "\"" + ";\n" +
        subs.map(_.toDot).mkString("\n")
    })
  }
    
}