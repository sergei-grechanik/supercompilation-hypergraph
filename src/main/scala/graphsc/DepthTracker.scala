package graphsc

trait DepthTracker extends Hypergraph {
  val depths = collection.mutable.Map[Node, Int]()
  
  def depth(n: RenamedNode): Int =
    depths(n.deref.node)
  
  def onDepthChanged(n: Node, d: Int) {}
  
  def updateDepth(n: Node, d: Int) {
    if(depths(n) > d && d >= 0) {
      depths(n) = d
      onDepthChanged(n, d)
      for(h <- n.outs) {
        h.dests.map(n => updateDepth(n.node, d + 1))
      }
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    val d = depths(h.source.node)
    h.dests.map(n => updateDepth(n.node, d + 1))
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    updateDepth(l.node, depths(r))
    super.beforeGlue(l, r)
  }
  
  override def onNewNode(n: Node) {
    depths(n) = Int.MaxValue
    super.onNewNode(n)
  }
}