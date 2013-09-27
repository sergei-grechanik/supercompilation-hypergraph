package graphsc
package transformation

case class PostFilter(graph: Hypergraph, filter: Hyperedge => Boolean) extends Hypergraph {
  val buffer = collection.mutable.Queue[Hyperedge]()
  
  override def newNode(u: Set[Int]): RenamedNode =
    RenamedNode.fromNode(new FreeNode(u))
  
  override def addHyperedge(h: Hyperedge) {
    buffer += h
  }
  
  override def log(s: String) { graph.log(s) }
  override def nodeToString(n: RenamedNode): String = graph.nodeToString(n)
  override def hyperedgeToString(h: Hyperedge): String = graph.hyperedgeToString(h)
  override def logShift() { graph.logShift() }
  override def logUnshift() { graph.logUnshift() }
  override def logTrans(name: String, hs: Seq[Hyperedge]) { graph.logTrans(name, hs) }
  
  override def trans(name: String, hs: Hyperedge*)(body: =>Unit) {
    graph.trans(name, hs: _*) {
      body
      if(buffer.forall(filter))
        buffer.foreach(graph.addHyperedge(_))
      else
        log("-- Denied by postfilter")
      buffer.clear()
    }
  }
}

case class HyperBuffer(graph: Hypergraph) extends Hypergraph {
  val buffer = collection.mutable.Queue[Hyperedge]()
  
  override def newNode(u: Set[Int]): RenamedNode =
    RenamedNode.fromNode(new FreeNode(u))
  
  override def addHyperedge(h: Hyperedge) {
    buffer += h
  }
  
  def commit() {
    buffer.foreach(graph.addHyperedge(_))
    buffer.clear()
  }
}