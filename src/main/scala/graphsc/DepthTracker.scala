package graphsc

trait DepthTracker extends Hypergraph {
  val depths = collection.mutable.Map[Node, Int]()
  val codepths = collection.mutable.Map[Node, Int]()
  
  // Measure of node's position, how deep it is in the graph
  def depth(n: RenamedNode): Int =
    depths(n.deref.node)
  
  // Measure of node's complexity, how deep its minimal definition is
  def codepth(n: RenamedNode): Int =
    codepths(n.deref.node)
    
  def onDepthChanged(n: Node, d: Int) {}
  def onCodepthChanged(n: Node, c: Int) {}
  
  def updateDepth(n: Node, d: Int) {
    if(depths(n) > d && d >= 0) {
      depths(n) = d
      onDepthChanged(n, d)
      for(h <- n.outs) {
        h.dests.map(n => updateDepth(n.node, d + 1))
      }
    }
  }
  
  def updateCodepth(n: Node, c: Int) {
    if(codepths(n) > c && c >= 0) {
      codepths(n) = c
      onCodepthChanged(n, c)
      for(h <- n.ins) {
        updateCodepth(h.source.node, (0 :: h.dests.map(d => codepths(d.node))).max + 1)
      }
    }
  }
  
  def zeroBoth(n: RenamedNode) {
    updateDepth(n.deref.node, 0)
    updateCodepth(n.deref.node, 0)
  }
  
  def limitDepth(maxd: Int): (Hyperedge, Hyperedge) => Boolean = {
    (h1,h2) =>
      nodesOf(h1,h2).map(depths(_)).max <= maxd
  }
  
  def limitCodepth(maxc: Int): (Hyperedge, Hyperedge) => Boolean = {
    (h1,h2) =>
      nodesOf(h1,h2).map(codepths(_)).max <= maxc
  }
  
  def limitDepthCodepth(f: (Int,Int) => Boolean):(Hyperedge, Hyperedge) => Boolean = {
    (h1,h2) =>
      val d = nodesOf(h1,h2).map(depths(_)).max
      val c = nodesOf(h1,h2).map(codepths(_)).max
      f(d,c)
  } 
  
  override def onNewHyperedge(h: Hyperedge) {
    val d = depths(h.source.node)
    h.dests.map(n => updateDepth(n.node, d + 1))
    updateCodepth(h.source.node, (0 :: h.dests.map(d => codepths(d.node))).max + 1)
    super.onNewHyperedge(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    updateDepth(l.node, depths(r))
    updateCodepth(l.node, codepths(r))
    super.beforeGlue(l, r)
  }
  
  override def onNewNode(n: Node) {
    depths(n) = Int.MaxValue
    codepths(n) = Int.MaxValue
    super.onNewNode(n)
  }
}