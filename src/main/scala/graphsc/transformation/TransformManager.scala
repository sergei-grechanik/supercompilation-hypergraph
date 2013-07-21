package graphsc
package transformation
import scala.util.Random

trait BiTransformManager extends Hypergraph with DepthTracker {
  var changed = true
  val updatedPairs = collection.mutable.Set[(Hyperedge, Hyperedge)]()
  
  // TODO: Find a better name
  def updateAll() {
    for(n <- allNodes; h1 <- n.insMut; h2 <- n.outsMut)
      updatedPairs.add((h1,h2))    
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    changed = true
    for(h1 <- h.source.node.insMut)
      updatedPairs.add((h1,h))
    for(d <- h.dests; h2 <- d.node.outsMut)
      updatedPairs.add((h,h2))
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(r: RenamedNode, n: Node) {
    changed = true
    for(h1 <- n.insMut; h2 <- r.node.outsMut)
      updatedPairs.add((h1,h2))
    for(h1 <- r.node.insMut; h2 <- n.outsMut)
      updatedPairs.add((h1,h2))
    super.beforeGlue(r, n)
  }
  
  override def onUsedReduced(n: Node) {
    changed = true
    for(h1 <- n.insMut; h2 <- n.outsMut)
      updatedPairs.add((h1,h2)) 
    super.onUsedReduced(n)
  }
  
  def filterUpdatedPairs(pairs: List[(Hyperedge, Hyperedge)]): List[(Hyperedge, Hyperedge)] = 
    pairs.sortBy(p => (depth(p._1.source), depth(p._2.source)))
  
  def transform(proc: BiHProcessor): Boolean = {
    val normalized = updatedPairs.map(p => (normalize(p._1), normalize(p._2)))
    updatedPairs.clear()
    updatedPairs ++= normalized
    //println("Pairs to transform before filtering: " + updatedPairs.size)
    val set = filterUpdatedPairs(updatedPairs.toList)
    updatedPairs --= set
    //println("Pairs to transform: " + set.size)
    var count = 0
    val processed = collection.mutable.Set[(Hyperedge, Hyperedge)]()
    for((h1o, h2o) <- set) {
      val h1 = normalize(h1o)
      val h2 = normalize(h2o)
      if(!processed((h1,h2)) && !processed((h1o,h2o)) 
          && h1.source.node.outsMut(h1) && h2.source.node.outsMut(h2)) {
        processed.add((h1, h2))
        proc(h1, h2)
        count += 1
      }
    }
    //println("Pairs transformed: " + count)
    updatedPairs.nonEmpty
  }
}


// This transformmanager is obsolete and should be replaced with 
// BiTransformManager wherever possible
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
  
  def transform(procHyperedge: HProcessor): Boolean = {
    // TODO: Shuffling may be good for testing but...
    //Random.shuffle(updatedHyperedges.map(_.deref).toList)//.sortBy(h => depth(h.source))
    val set = updatedHyperedges.map(normalize(_)).toList.sortBy(h => depth(h.source))
    updatedHyperedges.clear()
    val processed = collection.mutable.Set[Hyperedge]()
    for(ho <- set) {
      val h = normalize(ho)
      if(!processed(h) && !processed(ho) && h.source.node.outsMut(h)) {
        processed += h
        procHyperedge(h)
      }
    }
    updatedHyperedges.nonEmpty
  }
}
