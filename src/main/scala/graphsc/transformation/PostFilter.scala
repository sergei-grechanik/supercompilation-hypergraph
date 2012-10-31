package graphsc
package transformation

case class PostFilter(graph: Hypergraph, filter: Hyperedge => Boolean) extends Hypergraph {
  val buffer = collection.mutable.Queue[Hyperedge]()
  
  override def newNode(u: Set[Int]): RenamedNode =
    RenamedNode.fromNode(new FreeNode(u))
  
  override def addHyperedge(h: Hyperedge) {
    buffer += h
  }
  
  def commit() {
    if(buffer.forall(filter))
      buffer.foreach(graph.addHyperedge(_))
    
    buffer.clear()
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