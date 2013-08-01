package graphsc

class HyperedgeNotFoundException extends Exception

// A proxy that doesn't add hyperedges that are't already in the graph. 
class HyperedgeFinder(graph: NamedNodes) extends NamedNodes {
  override def normalize(h: Hyperedge): Hyperedge = graph.normalize(h)
  override def newNode(used: Set[Int]): RenamedNode = graph.newNode(used)
  
  override val namedNodes = graph.namedNodes
  
  override def newNode(n: String, arity: Int): RenamedNode = 
    if(namedNodes.contains(n)) {
      namedNodes(n).deref
    }
    else
      throw new HyperedgeNotFoundException
      
  override def addHyperedge(h1: Hyperedge) {
    val h = normalize(h1)
    if(!(h.label == Id() && h.source ~~ h.dests(0) && 
         (h.source.renaming.inv comp h.dests(0).renaming).isId(h.dests(0).node.used))) {
      if(h.dests.nonEmpty)
        h.dests.minBy(_.node.insMut.size).node.insMut
        .find(x => x.label == h.label && x.dests == h.dests) match {
          case Some(x) if h.source.node == x.source.node =>
          case Some(x) => graph.addHyperedge(h)
          case None =>
            if(h.label == Id() && h.source.node.isInstanceOf[FreeNode])
              graph.addHyperedge(h)
            else
              throw new HyperedgeNotFoundException
        }
      else
        graph.allHyperedges.find(x => x.dests.isEmpty && x.label == h.label) match {
          case Some(x) if h.source.node == x.source.node =>
          case Some(x) => graph.addHyperedge(h)
          case None => throw new HyperedgeNotFoundException
        }
    }
  }
}