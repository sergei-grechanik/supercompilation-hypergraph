package graphsc

sealed trait Label {
  def bound(destnum: Int): Set[Int] = Set()
}

case class Construct(name: String)
  extends Label

case class CaseOf(cases: List[(String, List[Int])])
  extends Label {
  override def bound(destnum: Int): Set[Int] = 
    if(destnum >= 1 && destnum <= cases.length)
      cases(destnum - 1)._2.toSet
    else
      Set()
}

case class Let(variable: Int)
  extends Label {
  assert(variable >= 0)
  
  override def bound(destnum: Int): Set[Int] =
    if(destnum == 0)
      Set(variable)
    else
      Set()
}

case class Tick
  extends Label

// Should be a permutation
case class Renaming(vector: Vector[Int])
  extends Label {
  assert(vector.toSet == (0 until vector.length).toSet)
  
  def comp(other: Renaming): Renaming = {
    val w = arity max other.arity
    val t1 = this.widen(w)
    val o1 = other.widen(w)
    Renaming(o1.vector.map(t1(_)))
  }
  
  def inv: Renaming = {
    val m = Map(vector.zipWithIndex : _*)
    Renaming(Vector() ++ (0 to m.keys.max).map(i => m.getOrElse(i, -1)))
  }
  
  def isId: Boolean =
    vector.zipWithIndex.forall{case (a,b) => a == b}
  
  def this(arity: Int) = this(Vector() ++ (0 until arity))
  def this(p: (Int, Int)) = 
    this(Vector() ++ (0 to (p._1 max p._2)).toArray.map(i => 
      if(i == p._1) p._2 else if(i == p._2) p._1 else i ))
  def this(i: Iterable[Int]) = this(Vector() ++ i)
  
  def arity: Int = vector.max + 1
  
  def widen(a: Int): Renaming =
    Renaming(Vector() ++ (0 to a).map(i => if(i < vector.length) vector(i) else i))
  
  def apply(i: Int): Int = vector(i)
}

case class Var
  extends Label

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  label match {
    case r: Renaming =>
      assert(r.arity >= dests(0).arity)
    case _ =>
  }
  
  def arity: Int = 
    if(dests.nonEmpty)
      dests.map(_.arity).max
    else if(label.isInstanceOf[Var])
      1
    else
      0
      
  
  def from(newsrc: Node): Hyperedge =
    Hyperedge(label, newsrc, dests)
  
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = if(source == old) n else source
    val newdst = 
      for(d <- dests)
        yield if(d == old) n else d
    Hyperedge(label, newsrc, newdst)
  }
}

class Node(val arity: Int) {
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
  
  def addNode(arity: Int): Node
  
  def removeNode(n: Node)
  
  def glueNodes(l: Node, r: Node): Node
}

class TheHypergraph extends Hypergraph {
  val nodes = collection.mutable.Set[Node]()
  
  override def addHyperedgeSimple(h: Hyperedge): Hyperedge = {
    if(h.source == null) {
      val n = new Node(h.arity)
      nodes += n
      val res = h.from(n)
      n.outs += res
      h.dests.foreach(_.ins.add(res))
      res
    }
    else {
      nodes += h.source
      h.source.outs += h
      h.dests.foreach(_.ins.add(h))
      h
    }
  }
  
  override def addHyperedge(h: Hyperedge): Hyperedge = {
    if(h.dests.nonEmpty)
      h.dests(0).ins.find(x => x.label == h.label && x.dests == h.dests) match {
        case Some(x) if h.source == null => x
        case Some(x) if h.source == x.source => x
        case Some(x) => glueNodes(h.source, x.source); h
        case None => addHyperedgeSimple(h)
      }
    else
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n); h
        case None => addHyperedgeSimple(h)
      }
  }
  
  override def addNode(arity: Int): Node = {
    val n = new Node(arity)
    nodes.add(n)
    n
  }
  
  override def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    for(h <- n.ins if h.source != n)
      h.source.outs -= h
    for(h <- n.outs; d <- h.dests if d != n)
      d.ins -= h
  }
  
  override def glueNodes(l1: Node, r1: Node): Node = {
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
  
  // glue parents recursively
  def afterGlue(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
  
  def toDot: String = {
    val sb = new StringBuilder()
    sb.append("digraph Hyper {\n")
    for(n <- nodes) {
      for(h <- n.outs) {
        sb.append("node \"" + h.toString + "\"[label=\"" + h.label.toString + "\"];\n")
        sb.append("\"" + n.toString + "\" -> \"" + h.toString + "\";\n")
        for(d <- h.dests)
          sb.append("\"" + h.toString + "\" -> \"" + d.toString + "\";\n")
      }
      sb.append("\n")
    }
    sb.append("}\n")
    sb.toString
  }
}
