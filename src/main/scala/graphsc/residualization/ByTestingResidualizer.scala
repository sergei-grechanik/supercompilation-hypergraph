package graphsc
package residualization

import graphsc.interpretation._
  
case class ProgramSubgraph(nodes: Set[Node], hyperedges: Map[Node, Hyperedge])

case class ByTestingResidualizer(graph: Hypergraph with HyperTester, autotestcount: Int = 1) {
    
  def apply(l: List[RenamedNode]): List[ProgramSubgraph] =
    golist(l, ProgramSubgraph(Set(), Map()))
  
  def apply(n: RenamedNode): List[ProgramSubgraph] =
    go(n.node, ProgramSubgraph(Set(), Map()))
  
  private def go(n: Node, already: ProgramSubgraph): List[ProgramSubgraph] = {
    if(already.nodes(n))
      List(already)
    else {
      // We start with 
      //val hss =
      //  graph.runCache(n).values.toList.sortBy(-_.cost).map(_.preferred) ++ 
      //    n.definingHyperedge.toList.map(List(_))
      val prefhs = graph.runCache(n).values.flatMap(_.preferred).toList
      // TODO: Sometimes there may be a dead code that hasn't been tested
      val hs = if(prefhs.isEmpty) n.definingHyperedge.toList else prefhs
      hs.map(graph.normalize(_)).distinct.flatMap(h =>
        golist(h.dests, ProgramSubgraph(already.nodes + n, already.hyperedges + (n -> h))))
    }
  }
  
  private def golist(ds: List[RenamedNode], cursub: ProgramSubgraph): List[ProgramSubgraph] =
    ds match {
      case Nil => List(cursub)  
      case d :: tl =>
        go(d.node, cursub).flatMap(golist(tl, _))
    }
  
  // Run an automatically generated test
  def autoTest(node: Node) {
    val trie = Trie.mkTrie(node.deref)
    val visited = collection.mutable.Set[Node]()
    var totest: List[(Int, Node, Trie, TrieSubst)] = Nil
    
    def go(n: Node, trie: Trie, subst: TrieSubst, depth: Int) {
      n.definingHyperedge match {
        case _ if visited.contains(n) =>
        case None => totest = (depth, n, trie, subst) :: totest
        case Some(h1) =>
          val h = graph.normalize(h1)
          visited += n
          h.label match {
            case Improvement() => go(h.dests(0).node, trie, subst, depth + 1)
            case Tick() => go(h.dests(0).node, trie, subst, depth + 1)
            case Var() => // we don't test variables
            case Construct(cname) =>
              val TrieConstr(tcname, ts) = trie.get
              assert(cname == tcname)
              for((d,t) <- h.dests zip ts)
                go(d.node, t, subst, depth + 1)
            case CaseOf(cs) =>
              val TrieCaseOf(v, cs1) = trie.get
              assert(cs.size == cs1.size)
              for((((n1,k1), (n2,k2,t)), d) <- 
                    cs.sorted zip cs1.sortBy(x => (x._1, x._2)) zip h.dests.tail) {
                assert(n1 == n2 && k1 == k2)
                val c = Map(v -> v.split(n1, k1))
                go(d.node, t, subst.mapValues(_.subst(c)) ++ c, depth + 1)
              }
            case _ =>
              throw new Exception("A strange defining hyperedge: " + h)
          }
      }
    }
    
    go(node, trie, Map(), 0)
    
    for((_, n, t, s) <- totest.sortBy(_._1)) {
      if(graph.runCache(n).isEmpty) {
        for(mintest <- findMinTest(t, autotestcount)) {
          val subst = s.mapValues(_.subst(mintest)) ++ mintest
          val args = (0 to node.arity).map(i => TrieVar(List(i)).subst(subst).toValue)
          graph.runNode(node.deref, args.toList)
          assert(graph.runCache(n).nonEmpty)
        }
      }
    }
  }
  
  def findMinTest(t: Trie, count: Int): List[TrieSubst] = {
    val queue = collection.mutable.Queue[(TrieSubst, List[Trie])]((Map(), List(t)))
    var res: List[TrieSubst] = Nil
    while(res.size < count && queue.nonEmpty) {
      val (s, t :: ts) = queue.dequeue()
      t match {
        case t:TrieThunk =>
          queue.enqueue((s, t.unrollMore(200) :: ts))
        case TrieBottom =>
          if(ts.isEmpty) res = s :: res
          else queue.enqueue((s, ts))
        case TrieVar(p) => 
          if(ts.isEmpty) res = s :: res
          else queue.enqueue((s, ts))
        case TrieConstr(_, as) =>
          if(as.isEmpty && ts.isEmpty) res = s :: res
          else queue.enqueue((s, as ++ ts))
        case TrieCaseOf(v, cs) =>
          for((n, k, t) <- cs) {
            val c = Map(v -> v.split(n, k))
            queue.enqueue((s.mapValues(_.subst(c)) ++ c, t :: ts.map(_.subst(c))))
          }
      }
    }
    res
  }
}
