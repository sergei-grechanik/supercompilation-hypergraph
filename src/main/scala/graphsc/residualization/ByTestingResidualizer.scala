package graphsc
package residualization

import graphsc.interpretation._
  
case class ProgramSubgraph(nodes: Set[Node], hyperedges: Map[Node, Hyperedge])

case class ByTestingResidualizer(graph: Hypergraph with HyperTester) {
    
  def apply(n: Node): List[ProgramSubgraph] =
    go(n, ProgramSubgraph(Set(), Map()))
  
  private def go(n: Node, already: ProgramSubgraph): List[ProgramSubgraph] = {
    if(already.nodes(n))
      List(already)
    else {
      val prefhs = graph.runCache(n).values.flatMap(_.preferred).toList
      // TODO: Sometimes there may be a dead code that hasn't been tested
      val hs = if(prefhs.isEmpty) n.definingHyperedge.toList else prefhs
      (for(h <- hs.map(graph.normalize(_)).distinct) yield {
        def procdests(cursub: ProgramSubgraph, ds: List[RenamedNode]): List[ProgramSubgraph] =
          ds match {
            case Nil => List(cursub)  
            case d :: tl =>
              go(d.node, cursub).flatMap(procdests(_, tl))
          }
        
        procdests(ProgramSubgraph(already.nodes + n, already.hyperedges + (n -> h)), h.dests)
      }).flatten
    }
  }
}
