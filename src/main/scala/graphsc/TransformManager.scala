package graphsc

trait TransformManager extends Hypergraph {
  val updatedHyperedges = collection.mutable.Set[Hyperedge]()
  
  def allHyperedges: Set[Hyperedge] = {
    val sets = allNodes.toList.map(n => n.ins ++ n.outs)
    (Set[Hyperedge]() /: sets)(_ | _)
  }
  
  // TODO: Find a better name
  def updateAll() {
    updatedHyperedges ++= allHyperedges
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    updatedHyperedges += h
    super.onNewHyperedge(h)
  }
  
  override def afterGlue(n: Node) {
    updatedHyperedges ++= n.outs
    super.afterGlue(n)
  }
  
  override def onUsedReduced(n: Node) {
    updatedHyperedges ++= n.outs
    updatedHyperedges ++= n.ins
    super.onUsedReduced(n)
  }
  
  def transforming(hs: Hyperedge*) {
    println("Nodes: " + allNodes.size)
    for(h <- hs)
      println("    " + h)
  }
  
  def transform(procHyperedge: Hyperedge => Unit) {
    val set = updatedHyperedges.map(_.derefGlued)
    updatedHyperedges.clear()
    val processed = collection.mutable.Set[Hyperedge]()
    for(h1 <- set; val h = h1.derefGlued; 
        if !processed(h) && !processed(h1) && allNodes(h.source) && h.source.outs(h)) {
      processed += h
      procHyperedge(h)
    }
  }
  
  def transform(procHyperedge: (Hyperedge, Hyperedge) => Unit) {
    transform(HyperedgePairProcessor(procHyperedge))
  }
}

object HyperedgePairProcessor { 
  def apply(f: (Hyperedge, Hyperedge) => Unit): Hyperedge => Unit = { 
    h =>
      for(d <- h.dests; h1 <- d.outs)
        f(h, h1)
      for(h1 <- h.source.ins)
        f(h1, h)
  }
}

object TransformationsToProcessor { 
  def apply(pairs: (String, PartialFunction[(Hyperedge, Hyperedge), Unit])*): 
    (Hyperedge, Hyperedge) => Unit =
      apply((_, _, _) => (), pairs:_*)
    
  def apply(
      beforeTrans : (Hyperedge, Hyperedge, String) => Unit,
      pairs: (String, PartialFunction[(Hyperedge, Hyperedge), Unit])*): 
    (Hyperedge, Hyperedge) => Unit = { 
    (h1, h2) =>
      for((name,trans) <- pairs)
        if(trans.isDefinedAt((h1,h2))) {
          beforeTrans(h1, h2, name)
          trans((h1,h2))
        }
  }
}
