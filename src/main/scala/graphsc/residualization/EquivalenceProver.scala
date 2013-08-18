package graphsc
package residualization

class EquivalenceProver[S, L](scc: SCC, likenesscalc: LikenessCalculator)
    (implicit cc: CorrectnessChecker[S]) {
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
    }).map(t => if(swap) t.swap else t)
  }
    
  def proveUncached(
      l: Node, 
      r: Node, 
      ren: Renaming = Renaming(), 
      hist: Hist = Nil): Option[EqProofTree] = {
    val lind = hist.indexWhere(_._1._1 == l)
    val rind = hist.indexWhere(_._2._1 == r)

    val renid = (ren | Renaming(r.used))
    
    if(l.deref ~=~ (ren comp r))
      Some(EqProofTree(ren, (l,r)))
    else if(l == r && renid.nonEmpty)
      renid.map(EqProofTree(_, (l,r)))
    else if(lind != -1 && rind != -1) {
      hist.find(p => p._1._1 == l && p._2._1 == r) match {
        case Some(((_, lsafety), (_, rsafety))) if cc.safe(lsafety) && cc.safe(rsafety) =>
          // I thought we should add some variables to ren (if they guarantee correctness)
          // But now I think they will be there if they guarantee correctness.
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
            lh <- lset; rh1 <- rset; 
            if lh.dests.size == rh1.dests.size;
            rh <- varySecond(lh, rh1)) yield
          likenessH(lh, rh, ren).map{ case (like, rn) => (like, lh, rh, rn) }
      
      val sorted_pairs = 
        pairs.collect{ case Some(p) => p }.toList.distinct.sortBy(-_._1)
      
      var result: Option[Renaming] = None
      var hypers: (Hyperedge, Hyperedge) = null
      var subtrees: List[EqProofTree] = null
        
      // for each pair of hyperedges while there is no result
      for((like, lh, rh, ren1) <- sorted_pairs; if result == None) {
        subtrees = Nil
        
        val newhisthead = ((l, cc(l.deref)), (r, cc(r.deref)))
        val newss1 =
          (newhisthead :: hist).map(p => cc.through(p._1._2, lh)).transpose
        val newss2 = 
          (newhisthead :: hist).map(p => cc.through(p._2._2, rh)).transpose
        
        var curren: Option[Renaming] = 
          Some(lh.source.renaming comp ren1 comp rh.source.renaming.inv)
        
        val lhs = lh.dests zip newss1 zip lh.shifts
        val rhs = rh.dests zip newss2
        
        for((((ld, news1), sh), (rd, news2)) <- lhs zip rhs if curren != None) {
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
            // It's let's head
            assert(lh.label == Let())
            
            // We cannot judge what variables are used if we've got a renaming from
            // the equivalence prover. Just FYI.
            prove(ld.node, rd.node, ld.renaming.inv comp rd.renaming, newhist) match {
              case None => curren = None
              case Some(t) => subtrees = t :: subtrees 
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
  
  // Since two let-expressions can match with some non-id rearrangement,
  // we should check them all.
  // This function builds a list of all viable rearranged versions of rh.
  def varySecond(lh: Hyperedge, rh: Hyperedge): List[Hyperedge] = {
    assert(lh.label == rh.label && lh.dests.size == rh.dests.size)
    if(lh.label != Let())
      List(rh)
    else {
      likeness(lh.dests(0), rh.dests(0)) match {
        case None => List()
        case Some((_, headren)) =>
          // this function creates a list of rearranged dest lists together with
          // corresponding rearranging renamings
          def nextDests(tail: List[(RenamedNode, Int)], rest: List[(RenamedNode, Int)]): 
            List[(List[RenamedNode], Renaming)] = tail match {
            case Nil => List((Nil, Renaming()))
            case (ld, i) :: tail1 =>
              // we try to pair the head of the rest part of the left hand side dests
              // with each of the node from the rest part of the right hand side dests
              (for((rd, j) <- rest 
                  if (headren(j) == -1 || headren(j) == i) &&
                     likeness(ld, rd) != None
                ) yield {
                // we've chosen the next node from the rhs dests,
                // do a recursive call and combine the renamings
                val nexts = nextDests(tail1, rest.filterNot(_._2 == j))
                nexts.map { case (ds, ren) => (rd :: ds, (ren | Renaming(j -> i)).get) }
              }).flatten
          }
          
          nextDests(lh.dests.tail.zipWithIndex, rh.dests.tail.zipWithIndex).map {
            case (ds, ren) =>
              Hyperedge(rh.label, rh.source, (ren comp rh.dests(0)) :: ds)
          }
      }
    }
  }
}

case class EqProofTree(
    renaming: Renaming, 
    nodes: (Node, Node), 
    out: Option[(Hyperedge, Hyperedge, List[EqProofTree])] = None) {
  
  def swap: EqProofTree =
    EqProofTree(renaming.inv, (nodes._2, nodes._1), 
        out.map { case (h1, h2, lst) => (h2, h1, lst.map(_.swap)) })
  
  // Why can we glue child nodes too, not only roots?
  // Because they are defined in terms of themselves and the roots.
  def performGluing(g: Hypergraph) {
    // The fisrt thing to do is to fix renamings
    propagateRenamings.performGluing1(g)
  }
  
  private def performGluing1(g: Hypergraph) {
    for((_,_,l) <- out; t <- l)
      t.performGluing(g)
    if(nodes._1.deref.node != nodes._2.deref.node)
      g.add(Id(), renaming comp nodes._2, List(nodes._1.deref))
  }
  
  // Fix renamings. When we prove equivalence we cannot infer correct renamings,
  // so we should essentially perform a data-flow analysis. 
  def propagateRenamings: EqProofTree = {
    var prev = this
    var cur = propagateRenamings1()
    while(cur != prev) {
      prev = cur
      cur = cur.propagateRenamings1()
    }
    cur
  }
  
  // One iteration of propagating renamings up
  private def propagateRenamings1(hist: List[EqProofTree] = Nil): EqProofTree = 
    out match {
      case None =>
        hist.find(t => t.nodes == nodes) match {
          case None =>
            // this node is refl in agda terms, we don't have to modify it since
            // equivalence prover use the set of used variables without questioning it
            this
          case Some(t) =>
            // it is a folding node, we should combine both renamings
            EqProofTree((t.renaming | renaming).get, nodes, None)
        }
      case Some((h1, h2, ds)) =>
        // recursively perform renaming propagation below
        val newds = ds.map(_.propagateRenamings1(this :: hist))
        // now lift through hyperedges and combine all the renamings
        var ren = Renaming()
        for((((d1, d2), s), t) <- h1.dests zip h2.dests zip h1.shifts zip newds if s != -1) {
          // if the renamings are uncombinable then something has gone wrong
          ren = (ren | (d1.renaming comp t.renaming comp d2.renaming.inv).unshift(s)).get
        }
        EqProofTree(
            h1.source.renaming.inv comp ren comp h2.source.renaming, 
            nodes, Some((h1, h2, newds)))
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
  
  def toLog(g: Hypergraph) {
    g.log("-- " + g.nodeToString(nodes._1.deref) + " = " + 
            g.nodeToString(renaming comp nodes._2.deref))
    out match {
      case None =>
      case Some((h1, h2, lst)) =>
        g.logShift()
        g.log("-- " + g.hyperedgeToString(h1))
        g.log("-- " + g.hyperedgeToString(h2))
        g.logShift()
        lst.foreach(_.toLog(g))
        g.logUnshift()
        g.logUnshift()
    }
  }
    
}