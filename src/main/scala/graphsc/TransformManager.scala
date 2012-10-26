package graphsc
import scala.util.Random

trait TransformManager2 extends Hypergraph with DepthTracker {
  private implicit def hyperOrd: Ordering[Hyperedge] =
    Ordering.by(h => h.arity)
  
  val updatedHyperedges = collection.mutable.PriorityQueue[Hyperedge]()
  
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
    updatedHyperedges ++= n.ins
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
  
  def transform(procHyperedge: Hyperedge => Unit): Boolean = {
    while(updatedHyperedges.nonEmpty) {
      val ho = updatedHyperedges.dequeue()
      val h = normalize(ho)
      if(h.source.node.outs(h)) {
        procHyperedge(h)
      }
    }
    updatedHyperedges.nonEmpty
  }
  
  def transform(procHyperedge: (Hyperedge, Hyperedge) => Unit): Boolean = {
    transform(HyperedgePairProcessor(procHyperedge))
  }
}



trait TransformManager extends Hypergraph with DepthTracker {
  val updatedHyperedges = collection.mutable.Set[Hyperedge]()
  
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
    updatedHyperedges ++= n.ins
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
  
  def transform(procHyperedge: Hyperedge => Unit): Boolean = {
    // TODO: Shuffling may be good for testing but...
    //Random.shuffle(updatedHyperedges.map(_.deref).toList)//.sortBy(h => depth(h.source))
    val set = updatedHyperedges.map(normalize(_)).toList.sortBy(h => depth(h.source))
    updatedHyperedges.clear()
    val processed = collection.mutable.Set[Hyperedge]()
    for(ho <- set) {
      val h = normalize(ho)
      if(!processed(h) && !processed(ho) && h.source.node.outs(h)) {
        processed += h
        procHyperedge(h)
      }
    }
    updatedHyperedges.nonEmpty
  }
  
  def transform(procHyperedge: (Hyperedge, Hyperedge) => Unit): Boolean = {
    transform(HyperedgePairProcessor(procHyperedge))
  }
}

object HyperedgePairProcessor { 
  def apply(f: (Hyperedge, Hyperedge) => Unit): Hyperedge => Unit = { 
    h =>
      // Note that we used to perform deref here, but not anymore
      // because deref is not enough, we must do either full normalization or nothing at all 
      for(d <- h.dests; h1 <- d.deref.node.outs)
        f(h, h1)
      for(h1 <- h.deref.source.node.ins)
        f(h1, h)
  }
}

object TransformationsToProcessor { 
  def apply(pairs: (String, PartialFunction[(Hyperedge, Hyperedge), Unit])*): 
    (Hyperedge, Hyperedge) => Unit =
      apply((_, _, _) => false, pairs:_*)
    
  def apply(
      whistle : (Hyperedge, Hyperedge, String) => Boolean,
      pairs: (String, PartialFunction[(Hyperedge, Hyperedge), Unit])*): 
    (Hyperedge, Hyperedge) => Unit = { 
    (h1o, h2o) =>
      for((h1,h2) <- transformablePairs(h1o, h2o))
        for((name,trans) <- pairs)
          if(trans.isDefinedAt((h1,h2))) {
            if(!whistle(h1o, h2o, name)) {
              trans((h1,h2))
            }
          }
  }
    
  def transformablePairs(h1: Hyperedge, h2: Hyperedge): List[(Hyperedge, Hyperedge)] = {
    val Hyperedge(l1, src1, ds1) = h1
    val Hyperedge(l2, src2, ds2) = h2
    // we restore the original used set because it makes it easier to transform
    val node = new Node(src2.used | h2.used)
    val rnode = RenamedNode(src2.renaming.inv, node)
    
    val maxvar = h1.arity + h1.shifts.max
    
    // as there may be several occurences of src2 in ds1, we should consider them one by one
    val pairs = 
      for((d,i) <- ds1.zipWithIndex if d.node == src2.node) yield {
        val newds1 = 
          ds1.zipWithIndex.map { 
            case (d,j) => 
              if(i == j) (d.renaming comp rnode).restoreUnused(maxvar)._2
              else d
          }
        (Hyperedge(l1, src1, newds1), Hyperedge(l2, RenamedNode.fromNode(node), ds2))
      }
    
    // We glue it here because otherwise we wouldn't have been able to correctly restore unused
    // variables. We could do without this gluing but some transformations use this dummy node
    // as if it weren't dummy.
    node.gluedTo = src2
    
    pairs
  }
}
