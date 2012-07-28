package graphsc

sealed trait Label {
  def bound(destnum: Int): Set[Int] = Set()
}

case class Construct(name: String)
  extends Label

case class CaseOf(cases: List[(String, Int)])
  extends Label{
  override def bound(destnum: Int): Set[Int] = 
    if(destnum >= 1 && destnum <= cases.length)
      (0 until cases(destnum - 1)._2).toSet
    else
      Set()
}

case class Let(variable: Int)
  extends Label{
  override def bound(destnum: Int): Set[Int] =
    if(destnum == 0)
      Set(variable)
    else
      Set()
}

case class Tick
  extends Label
  
case class Renaming(vector: Array[Int])
  extends Label {
  
  def comp(other: Renaming): Renaming =
    Renaming(other.vector.map(vector(_)))
  
  def inv: Renaming =
    new Renaming(Map(vector zip (0 until vector.length) : _*))
    
  def this(arity: Int) = this((0 until arity).toArray)
  def this(m: Map[Int, Int]) = this((0 to m.keys.max).toArray.map(i => m.getOrElse(i, i)))
  def this(i: Iterable[Int]) = this(i.toArray)
  
  def arity: Int = vector.max + 1
  
  def apply(i: Int): Int = vector(i)
}

case class Var
  extends Label

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  def arity: Int = label match {
    case Let(x) =>
      if(x >= dests(0).arity)
        dests(0).arity max dests(1).arity
      else
        (dests(0).arity - 1) max dests(1).arity
    case CaseOf(cases) => 
      (dests(0).arity :: (dests.tail zip cases).map{case (l,r) => l.arity - r._2}).max
    case _ => dests.map(_.arity).max
  }
  
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
  
  override def removeNode(n: Node) {
    nodes -= n
    // we should leave n.outs and n.ins intact
    for(h <- n.ins if h.source != n)
      h.source.outs -= h
    for(h <- n.outs; d <- h.dests if d != n)
      d.ins -= h
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
  def letDown(g: Hypergraph, let: Hyperedge) = let match {
    case Hyperedge(Let(x), src, List(f, e)) =>
      for(h <- f.outs) h.label match {
        case ren : Renaming =>
          val ner = ren.inv
          if(x >= ner.arity)
            g.addHyperedge(h.from(src))
          else {
            val newe = g.addHyperedge(Hyperedge(ner, null, List(e))).source
            val newlet = g.addHyperedge(Hyperedge(Let(ner(x)), null, List(h.dests(0), newe))).source
            g.addHyperedge(Hyperedge(ren, src, List(newlet)))
          }
        case lab =>
          
      }
    case _ =>
  }
}
