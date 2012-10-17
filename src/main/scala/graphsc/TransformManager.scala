package graphsc
import scala.util.Random

trait TransformManager extends Hypergraph with DepthTracker {
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
    val set = Random.shuffle(updatedHyperedges.map(_.deref).toList).sortBy(h => depth(h.source))
    updatedHyperedges.clear()
    val processed = collection.mutable.Set[Hyperedge]()
    for(h1 <- set; val h = canonize(normalize(h1.reduceDestRenamings))._2; 
        if !processed(h) && !processed(h1) && 
            allNodes(h.source.node) && h.source.node.outs(h)) {
      processed += h
      procHyperedge(h)
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
    val node = new Node(src2.used)
    node.gluedTo = src2
    val rnode = RenamedNode(src2.renaming.inv, node)
    
    // as there may be several occurences of src2 in ds1, we should consider them one by one
    for((d,i) <- ds1.zipWithIndex if d.node == src2.node) yield {
      val newds1 = ds1.zipWithIndex.map { case (d,j) => if(i == j) d.renaming comp rnode else d }
      (Hyperedge(l1, src1, newds1), Hyperedge(l2, RenamedNode.fromNode(node), ds2))
    }
  }
}
