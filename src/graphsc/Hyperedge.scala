package graphsc

sealed trait Label

case class Construct(name: String)
  extends Label

case class CaseOf(cases: List[(String, Int)])
  extends Label

case class Let
  extends Label

case class Tick
  extends Label
  
case class Id
  extends Label

case class Var
  extends Label

// Negative numbers represent bound variables
case class Renaming(vector: Array[Int]) {
  def comp(other: Renaming): Renaming = {
    val boundshift = (0 :: vector.filter(_ < 0).toList).min
    Renaming(other.vector.map(x =>
        if(x < 0) x + boundshift else vector(x)))
  }
  
  def comp(e: Edge): Edge = e match {
    case Edge(n, r) => Edge(n, this comp r)
  }
  
  def this(arity: Int) = this((0 until arity).toArray)
  def this(m: Map[Int, Int]) = this((0 to m.keys.max).toArray.map(i => m.getOrElse(i, i)))
  def this(i: Iterable[Int]) = this(i.toArray)
  
  def arity: Int = vector.max + 1
  
  def apply(i: Int): Int = vector(i)
  
  // Split the renaming into a simple binding renaming (bind first n variable)
  // and the rest part which doesn't perform binding.
  def splitBound: (Renaming, Renaming) = {
    val boundshift = (0 :: vector.filter(_ < 0).toList).min
    val binder = Renaming(boundshift until arity)
    val tail = Renaming(vector.map(_ - boundshift))
    (binder, tail)
  }
    
}



case class Edge(node: Node, renaming: Renaming) {
  def arity: Int = renaming.arity
}

case class Hyperedge(label: Label, source: Node, dests: List[Edge]) {
  def arity: Int = label match {
    case Let() => (dests(0).arity - 1) max dests(1).arity
    case CaseOf(cases) => 
      (dests(0).arity :: (dests.tail zip cases).map{case (l,r) => l.arity - r._2}).max
    case _ => dests.map(_.arity).max
  }
  
  def from(newsrc: Node): Hyperedge =
    Hyperedge(label, newsrc, dests)
  
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = if(source == old) n else source
    val newdst = 
      for(e <- dests)
        yield Edge(if(e.node == old) n else e.node , e.renaming)
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
    
  def toEdge =
    Edge(getRealNode, new Renaming(arity))
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
      val n = new Node(h.arity)
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
      nodes.find(_.outs.exists(_.label == h.label)) match {
        case Some(n) if h.source == null => h.from(n)
        case Some(n) => glueNodes(h.source, n); h
        case None => addHyperedgeSimple(h)
      }
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
  
  // glue parents recursively
  def afterGlue(n: Node) {
    val groups = n.ins.groupBy(h => (h.label, h.dests)).filter(_._2.size > 1)
    for((_, g) <- groups)
      g.toList.map(_.source).reduce(glueNodes)
  }
}

object Transformations {
  // let e in (a + b) -> (let e in a) + (let e in b)
  // let x = e in x -> e
  def letDown(g: Hypergraph, let: Hyperedge): Boolean = let match {
    case Hyperedge(Let(), src, List(ab, e)) =>
      val theta = ab.renaming
      for(h <- ab.node.outs) {
        if(h.label == Var() && theta(0) == -1)
          g.addHyperedge(Hyperedge(Id(), src, List(e)))
        else {
          val newdests = 
            h.dests.map(d => 
              g.addHyperedge(Hyperedge(let.label, null, List(theta comp d, e))).source.toEdge)
          g.addHyperedge(Hyperedge(h.label, src, newdests))
        }
      }
      true
    case _ => false
  }
  
  // case x of { S y -> d }  ->  case x of { S y -> let x = S y in d } 
  def propagate(g: Hypergraph, cas: Hyperedge): Boolean = cas match {
    case Hyperedge(CaseOf(cases), src, (x@Edge(xnode, xren)) :: dests)
      if xnode.outs.exists(_.label == Var()) =>
        val xnum = xren(0)
        val newdests = dests zip cases map {
          case (d, (c, n)) =>
            d.renaming.back(xnum) match {
              case None => d // the variable x is not used by this case
              case Some(_) =>
                val (bnd, tl) = d.renaming.splitBound
                // xnum in d.renaming is xnum + n in tl
                // bnd should bind exactly n variables
                // I know, it's all vague crazyness and should be controlled by types
                val underlet = Renaming(Map(xnum + n -> -1)) comp d
                g.addHyperedge(Hyperedge(Let(), null, List(newd, )))
            }
        }
  }
}
