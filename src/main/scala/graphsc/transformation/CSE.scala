package graphsc
package transformation

import graphsc.residualization._

object CSEFramework extends DataflowFramework[Option[Set[RenamedNode]]] {
  override def init(n: Node): Type = None
  override def top: Type = Some(Set())
  override def meet(a: Type, b: Type): Type = a.flatMap(x => b.flatMap(y => Some(x | y)))
  override def trans(h: Hyperedge, l: List[Type]): Type = h.label match {
    case Var() => None
    case lbl if lbl.isSimple && l.forall(_.isEmpty) => None
    case _ =>
      Some(destSubexpressions(h, l).toSet + h.source.node.deref)
  }

  def destSubexpressions(h: Hyperedge, l: List[Type]): Traversable[RenamedNode] = {
    for((set, shift, dest) <- (l.map(_.getOrElse(Set())), h.shifts, h.dests).zipped; 
        n1 <- set; n = dest.renaming comp n1;
        if shift >= 0 && n.used.forall(_ >= shift)) 
      yield h.source.renaming.inv comp n.shiftVars(-shift)
  }
}

class CSE(graph: Hypergraph) {
  val subexpressions = DataflowAnalyzer(CSEFramework)(graph)

  def subexpressions1(node: Node): Set[RenamedNode] =
    subexpressions.get(node).flatten.getOrElse(Set[RenamedNode]())

  def printCandidates() {
    for(n <- graph.allNodes; if !subexpressions(n).isEmpty; h <- n.outsUnderef;
        if !h.label.isInstanceOf[CaseOf] || true) {
      val map = collection.mutable.Map[RenamedNode, Int]().withDefault(_ => 0)
      for(sub <- CSEFramework.destSubexpressions(h, 
                    h.dests.map(d => subexpressions.get(d.node).flatten))) {
        map(sub) += 1
      }
      for((sub, k) <- map; if k > 1; if sub.used.nonEmpty;
          if !h.label.isInstanceOf[CaseOf] || subexpressions1(h.dests(0).node)(sub)) {
        System.err.println("cse candidate from node: ")
        System.err.println(n.prettyDebug)
        System.err.println(graph.hyperedgeToString(h))
        System.err.println("subexpression: ")
        // System.err.println(graph.nodeToString(sub))
        System.err.println(sub.node.prettyDebug)
        System.err.println()

        factorOut(h, sub)
      }
    }
  }

  def factorOut(where: Hyperedge, what: RenamedNode) {
    val holed = makeHole(where, what, Set[Node]())
    val maxi = (where.source.node.used + (-1)).max
    for(n <- holed) {
      graph.add(Let(), where.source.node.deref, 
          List(n) ++ (0 to maxi).map(graph.variable(_)).toList ++ List(what))
    }
  }

  def makeHole(where: Node, what: RenamedNode,
               history: Set[Node]): List[RenamedNode] = {
    val holei = (where.used + (-1)).max + 1
    val res =
      if(!subexpressions(where).getOrElse(Set())(what) || history(where))
        List(where.deref)
      else if(where.deref == what.deref)
        List(graph.variable(holei))
      else
        (for(h <- where.outsUnderef; n <- makeHole(h, what, history + where)) yield n).distinct

    // System.err.println("hole for node: ")
    // System.err.println(where.prettyDebug)
    // for(r <- res) {
    //   System.err.println(graph.nodeToString(r))
    //   System.err.println(r.node.prettyDebug)
    // }

    res
  }

  def makeHole(where: Hyperedge, what: RenamedNode, 
               history: Set[Node]): List[RenamedNode] = {
    val srcholei = (where.source.node.used + (-1)).max + 1
    val overallholei = (where.source.used + (-1)).max + 1
    val newdests =
      for((dest, shift) <- (where.dests, where.shifts).zipped) yield { 
        if(shift < 0) List(dest)
        else {
          val holei = (dest.node.used + (-1)).max + 1
          val holed =
            makeHole(dest.node, 
              dest.renaming.inv comp (where.source.renaming comp what).shiftVars(shift),
              history)
          holed.map(hd => (dest.renaming + (holei -> (overallholei + shift))) comp hd)
        }
      }
    for(dests <- sequence(newdests.toList)) yield {
      val almost = graph.add(where.label, dests)
      // System.err.println("hole for hyperedge: ")
      // System.err.println(graph.hyperedgeToString(where))
      // System.err.println("almost: ")
      // System.err.println(graph.nodeToString(almost))
      val newren = where.source.renaming.inv + (overallholei -> srcholei)
      // System.err.println("source node: ")
      // System.err.println(where.source.node.prettyDebug)
      // System.err.println("result: ")
      // System.err.println(graph.nodeToString(newren comp almost))
      newren comp almost
    }
  }

}
