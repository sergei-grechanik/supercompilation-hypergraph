package graphsc

sealed trait Label

case class Construct(name: String)
  extends Label

case class Scrutinize(cases: List[(String, Int)])
  extends Label

case class Let
  extends Label
  
case class Id
  extends Label

// It is actually a permutation, it shouldn't glue variables
case class Renaming(vector: Vector[Int]) {
  def comp(other: Renaming): Renaming =
    Renaming(other.vector.map(vector(_)))
}



case class Edge(node: Node, renaming: Renaming, tick: Boolean = false)

case class Hyperedge(label: Label, source: Node, dests: List[Edge]) {
  def from(newsrc: Node): Hyperedge =
    Hyperedge(label, newsrc, dests)
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = if(source == old) n else source
    val newdst = 
      for(e <- dests)
        yield Edge(if(e.node == old) n else e.node , e.renaming, e.tick)
    Hyperedge(label, newsrc, newdst)
  }
}

class Node {
  val outs = collection.mutable.Set[Hyperedge]()
  val ins = collection.mutable.Set[Hyperedge]()
  var gluedTo: Node = null
  
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def getRealNode: Node =
    if(gluedTo == null) this
    else gluedTo.getRealNode
}

trait Hypergraph {
  // h should be with known dests and null source
  // if source is not null then perform gluing
  def addHyperedge(h: Hyperedge): Hyperedge
  
  def addHyperedgeSimple(h: Hyperedge): Hyperedge
  
  def removeNode(n: Node)
  
  def glueNodes(l: Node, r: Node): Node
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    if(h.source == null) {
      val n = new Node
      nodes += n
      val res = h.from(n)
      n.outs += res
      h.dests.foreach(_.node.ins.add(res))
      res
    }
    else {
      nodes += h.source
      h.source.outs += h
      h.dests.foreach(_.node.ins.add(h))
      h
    }
  }
  
  override def addHyperedge(h: Hyperedge): Hyperedge = {
    if(h.dests.nonEmpty)
      h.dests(0).node.ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source); h
        case None => addHyperedgeSimple(h)
      }
    else
      addHyperedgeSimple(h)
  }
  
  override def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    for(h <- n.ins if h.source != n)
      h.source.outs -= h
    for(h <- n.outs; d <- h.dests if d.node != n)
      d.node.ins -= h
  }
  
  override def glueNodes(l1: Node, r1: Node): Node = {
    // Don't know if we should allow this
    val l = l1.getRealNode
    val r = r1.getRealNode
    if(nodes.contains(l) && nodes.contains(r) && l != r) {
      removeNode(r)
      r.gluedTo = l
      for(h <- r.outs)
        addHyperedgeSimple(h.replace(r, l))
      for(h <- r.ins)
        addHyperedgeSimple(h.replace(r, l))
      // maybe there appeared some more nodes to glue 
      afterGlue(l)
      // Now l may be glued to something else
      l.getRealNode
    }
    else if(l == r)
      l // Nodes are already glued
    else
      // Well, you shouldn't do this, they don't belong to this graph
      throw new IllegalArgumentException("Cannot glue nodes which aren't in this graph")
      // But maybe we should return null here, I'm not sure
  }
  
  def afterGlue(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
}

object Hypergraph { 
}
