package graphsc
package transformation
import scala.util.Random

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
      if(!processed(h) && !processed(ho) && h.source.node.outs(h)) {
        processed += h
        procHyperedge(h)
      }
    }
    updatedHyperedges.nonEmpty
  }
}
