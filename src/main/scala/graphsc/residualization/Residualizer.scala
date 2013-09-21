package graphsc
package residualization
  
class Residualizer[S](graph: Hypergraph, scc: SCC)
    (implicit cc: CorrectnessChecker[S]) {
  
  var misses: Int = 0
  val cache = collection.mutable.Map[(Node, List[(Node, S)]), Option[ResidualTree]]()
  val minbounds = collection.mutable.Map[(Node, List[(Node, S)]), Int]()
  
  def apply(n: RenamedNode, maxbnd: Int = Int.MaxValue): Option[ResidualTree] =
    go(n.node, Nil, maxbnd)
  
  private def cost(h: Hyperedge): Int = h.label match {
    case Var() => 1
    case Unused() => 1
    case Construct(_) => 2
    case Id() => 2
    case Tick() => 2
    case Improvement() => 3
    case CaseOf(_) if h.dests(0).getVarUnused.isDefined => 5
    case CaseOf(_) => 10
    case Let() => 30
  }
    
  private def prio(h: Hyperedge) = h.dests.size
    
  private def go(n: Node, hist1: List[(Node, S)], maxbnd: Int): Option[ResidualTree] = {
    if(maxbnd <= 0)
      return None
      
    val hist = hist1.takeWhile(p => scc.componentOf(p._1) == scc.componentOf(n))
    //goUncached(n, hist, maxbnd)
    cache.get((n, hist)) match {
      case Some(r) => r.filter(_.cost <= maxbnd)
      case None =>
        if(minbounds.getOrElse((n,hist), 0) < maxbnd) {
          misses += 1
          val res = goUncached(n, hist, maxbnd)
          if(res != None)
            cache += (n,hist) -> res
          else
            minbounds((n,hist)) = maxbnd
          res
        }
        else
          None
    }
    //cache.getOrElseUpdate((n, hist), goUncached(n, hist, Int.MaxValue)).filter(_.size <= maxbnd)
  }
    
  private def goUncached(n: Node, hist: List[(Node, S)], maxbnd: Int): Option[ResidualTree] = {
    if(maxbnd <= 0)
      return None
      
    hist.find(_._1 == n) match {
      case Some((_, s)) if cc.safe(s) =>
        val node = graph.newNode(n.used)
        Some(ResidualTree(node, 1, List(n -> node)))
      case Some((_, s)) =>
        None
      case None =>
        var curbnd = maxbnd
        val results =
          (for(h <- n.outs.toList.sortBy(prio(_)) if curbnd != 0) yield {
            val newhists = cc.histThrough((n, cc(n.deref)) :: hist, h)
            var curchldbnd = curbnd - cost(h)
            val child = 
              (h.dests, newhists).zipped.map {
                  (d,hst) => 
                    val res = go(d.node, hst, curchldbnd)
                    if(res.nonEmpty)
                      curchldbnd -= res.get.cost
                    else
                      curchldbnd = 0
                    res
                }
            if(child.forall(_.isDefined)) {
              val childget = child.map(_.get)
              val res = (childget.map(_.cost).sum + cost(h), h, child.map(_.get))
              if(res._1 <= curbnd) {
                curbnd = res._1 - 1
                Some(res)
              } else
                None
            }
            else
              None
          }) collect { case Some(rt) => rt }
        
        if(results.isEmpty)
          None
        else {
          val (cst, h, children) = results.last
          val dests = (h.dests, children).zipped.map((d,c) => d.renaming comp c.node)
          val resnode = h.source.renaming.inv comp graph.add(h.label, dests)
          
          val resmap = 
            (List(n -> resnode) :: children.map(_.folds))
              .flatten.groupBy(_._1).mapValues(l => graph.glue(l.map(_._2)))
          
          Some(ResidualTree(resnode, cst, resmap.toList.filter(_._1 != n)))
        }
    }
  }
}

case class ResidualTree(node: RenamedNode, cost: Int, folds: List[(Node, RenamedNode)])

/*
sealed trait ResidualTree {
  def size: Int = this match {
    case RTHyperedge(h, ch) => 1 + (0 :: ch.map(_.size)).sum
    case RTFold(_) => 1
    case RTCall(_) => 1
  }
  
  def foldsToCalls(rt: ResidualTree, hist: List[ResidualTree] = Nil): ResidualTree = rt match {
    case RTHyperedge(h, cs) =>
      RTHyperedge(h, cs.map(foldsToCalls(_, rt :: hist)))
    case RTFold(n) =>
      RTCall(hist.collectFirst{ case r@RTHyperedge(h, _) if h.source.node == n => r}.get)
    case RTCall(_) => rt
  }
  
  def extractSharing: List[ResidualTree] = {
    var occurrences = collection.mutable.Map[ResidualTree, Int]()
    
    val better_rt = foldsToCalls(this)
    go(better_rt)
    
    def go(rt: ResidualTree): Unit = rt match {
      case RTHyperedge(h, cs) =>
        occurrences(rt) = occurrences.getOrElse(rt, 0) + 1
        cs.foreach(go(_))
      case RTCall(rt) =>
        occurrences(rt) = occurrences.getOrElse(rt, 0) + 1
      case RTFold(n) =>
        assert(false)
    }
    
    def foldsToCalls(rt: ResidualTree, hist: List[ResidualTree] = Nil): ResidualTree = rt match {
    case RTHyperedge(h, cs) =>
      RTHyperedge(h, cs.map(foldsToCalls(_, rt :: hist)))
    case RTFold(n) =>
      RTCall(hist.collectFirst{ case r@RTHyperedge(h, _) if h.source.node == n => r}.get)
    case RTCall(_) => rt
    }
    
    Nil
  }
}

case class RTHyperedge(hyperedge: Hyperedge, children: List[ResidualTree]) extends ResidualTree
case class RTFold(node: Node) extends ResidualTree
case class RTCall(fun: ResidualTree) extends ResidualTree*/
