package graphsc
package residualization
  
abstract class GenericResidualizer[S, R](scc: SCC)
    (implicit cc: CorrectnessChecker[S]) {
  
  var misses: Int = 0
  val cache = collection.mutable.Map[(Node, List[(Node, S)]), R]()
  
  def apply(n: RenamedNode): R =
    go(n.node, Nil)
  
  def priority(h: Hyperedge): Int = h.dests.size
    
  def go(n: Node, hist1: List[(Node, S)]): R = {
    val hist = hist1.takeWhile(p => scc.componentOf(p._1) == scc.componentOf(n))
    //goUncached(n, hist, maxbnd)
    cache.get((n, hist)) match {
      case Some(r) => r
      case None =>
          misses += 1
          val res = goUncached(n, hist)
          if(res != None)
            cache += (n,hist) -> res
          res
    }
  }
  
  def foldSafe(n: Node, s: S): R
  def foldUnsafe(n: Node, s: S): R
  def throughHyperedge(h: Hyperedge, children: List[() => R]): R
  def combine(n: Node, i: List[() => R]): R  
  
  def goUncached(n: Node, hist: List[(Node, S)]): R = {
    hist.find(_._1 == n) match {
      case Some((_, s)) if cc.safe(s) =>
        foldSafe(n, s)
      case Some((_, s)) =>
        foldUnsafe(n, s)
      case None =>
        val results =
          for(h <- n.outs.toList.sortBy(priority(_))) yield () => {
            val newhists = cc.histThrough((n, cc(n.deref)) :: hist, h)
            val child = 
              (h.dests, newhists).zipped.map {
                  (d,hst) => () => go(d.node, hst)
                }
            throughHyperedge(h, child)
          }
        combine(n, results)
    }
  }
}

case class ResidualSubtree(
    node: Node, out: Set[(Hyperedge, List[ResidualSubtree])], folded: Boolean = false) {
  def allNodes: Set[Node] = 
    out.flatMap(_._2.flatMap(_.allNodes)) + node ++ out.flatMap(_._1.dests.map(_.node))
  def allHyperedges: Set[Hyperedge] = out.flatMap(_._2.flatMap(_.allHyperedges)) ++ out.map(_._1)
  def foldings: Set[Node] =
    out.flatMap(_._2.flatMap(_.foldings)) - node ++ Set(node).filter(_ => folded) 
  lazy val closed = foldings.isEmpty
  
  def loadInto(graph: Hypergraph, map: Map[Node, RenamedNode] = Map()): RenamedNode = {
    if(folded) 
      map(node)
    else {
      val newnode = graph.newNode(node.used)
      val newmap = map + (node -> newnode)
      for((h,rs) <- out) {
        val rslen = rs.size
        val newds =
          for((d,i) <- h.dests.zipWithIndex) yield {
            if(i < rslen) d.renaming comp rs(i).loadInto(graph, newmap)
            else graph.newNode(d.used)
          }
        graph.add(h.label, h.source.renaming comp newnode, newds)
      }
      newnode
    }
  }
  
  def loadInto2(graph: Hypergraph): RenamedNode = {
    val cache =
      collection.mutable.Map[ResidualSubtree, RenamedNode]()
      
    def go(subtree: ResidualSubtree, 
           graph: Hypergraph, map: Map[Node, RenamedNode]): RenamedNode = {
      if(folded) 
        map(node)
      else if(subtree.closed && cache.contains(subtree)) 
        cache(subtree)
      else {
        val newnode = graph.newNode(node.used)
        val newmap = map + (node -> newnode)
        for((h,rs) <- out) {
          val rslen = rs.size
          val newds =
            for((d,i) <- h.dests.zipWithIndex) yield {
              if(i < rslen) d.renaming comp rs(i).loadInto(graph, newmap)
              else graph.newNode(d.used)
            }
          graph.add(h.label, h.source.renaming comp newnode, newds)
        }
        if(subtree.closed)
          cache += subtree -> newnode
        newnode
      }
    }
    
    go(this, graph, Map())
  }
}
    
class ConstructorsResidualizer[S](scc: SCC)(implicit cc: CorrectnessChecker[S])
    extends GenericResidualizer[S, ResidualSubtree](scc)(cc) {
  type R = ResidualSubtree
  override def foldSafe(n: Node, s: S): R = 
    ResidualSubtree(n, Set(), true)
  override def foldUnsafe(n: Node, s: S): R = 
    ResidualSubtree(n, Set())
  
  def mkSubtree(h: Hyperedge): ResidualSubtree =
    ResidualSubtree(h.source.node, Set((h, Nil)))
    
  override def throughHyperedge(h: Hyperedge, children: List[() => R]): R = h.label match {
    case Construct(name) => mkSubtree(h)
    case Unused() => mkSubtree(h)
    case Var() => mkSubtree(h)
    case CaseOf(cases) =>
      val chlds = children.map(_())
      ResidualSubtree(h.source.node, Set((h, chlds)))
    case _ => // including Let
      val chld = children(0)()
      if(chld.out.isEmpty)
        ResidualSubtree(h.source.node, Set())
      else
        ResidualSubtree(h.source.node, Set((h, List(chld))))
  }
  
  override def combine(n: Node, i: List[() => R]): R  = {
    var cur = ResidualSubtree(n, Set())
    for(f <- i; r = f()) yield {
      if(r.out.nonEmpty) {
        val l = r.out.head._1.label
        if(l.isInstanceOf[Var] || l.isInstanceOf[Unused] || l.isInstanceOf[Construct])
          return r
      }
      
      cur = ResidualSubtree(n, cur.out ++ r.out, cur.folded || r.folded)
    }
    cur
  }
}

/*
case class ConstructorSet[V](set: Set[(Option[String], Set[V])]) {
  def |(cs: ConstructorSet[V]): ConstructorSet[V] =
    ConstructorSet(set | cs.set)
  
  def &(cs: ConstructorSet[V]): ConstructorSet[V] = ConstructorSet( 
    for((c1, ns1) <- set; (c2, ns2) <- cs.set; if c1 == c2 || c1.isEmpty || c2.isEmpty) yield
      (c1.orElse(c2), ns1 | ns2)
  )
  
  def simplify: ConstructorSet[V] = ConstructorSet( 
    for((c, ns) <- set; 
        if !set.exists(p => (p._1 == c || p._1.isEmpty) && p._2.subsetOf(ns))) 
      yield (c, ns)
  )
  
  def replace(n: V, wth: ConstructorSet[V]): ConstructorSet[V] = ConstructorSet(
    set.flatMap { case (c, ns) =>
      if(ns.contains(n)) {
        (ConstructorSet(Set((c, ns - n))) & wth).set
      } else {
        Set((c, ns))
      }
    }
  )
  
  def fix(n: V, init: ConstructorSet[V]): ConstructorSet[V] = {
    var cur = this.replace(n, init).simplify
    var changed = true
    while(changed) {
      changed = false
      val newcur = cur.replace(n, cur).simplify
      if(newcur != cur) {
        cur = newcur
        changed = true
      }
    }
    cur
  }
}

class ConstructorsResidualizer[S](scc: SCC)(implicit cc: CorrectnessChecker[S])
    extends GenericResidualizer[S, ConstructorSet[Node]](scc)(cc) {
  type R = ConstructorSet[Node]
  override def foldSafe(n: Node, s: S): R = ConstructorSet(Set((None, Set(n))))
  override def foldUnsafe(n: Node, s: S): R = ConstructorSet(Set((None, Set())))
  
  override def throughHyperedge(h: Hyperedge, children: List[() => R]): R = h.label match {
    case Construct(name) => ConstructorSet(Set((Some(name), Set())))
    case CaseOf(cases) =>
      val ctrs = l(0).getOrElse(cases.map(_._1).toSet)
      val res = (Option(Set[String]()) /: (cases zip l.tail).filter(p => ctrs(p._1._1)).map(_._2))(combine)

      res
    case Unused() => Some(Set())
    case Var() => None
    case Let() => l(0)
    case _ => l(0)
  }
  
  override def combine(i: List[() => R]): R  
}
*/
