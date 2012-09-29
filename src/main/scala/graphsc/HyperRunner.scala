package graphsc

// Simple strict node runner, doesn't cache results, doesn't perform nontermination checks
object HyperRunner {
  def run(n: RenamedNode, as: List[Value]): Value = {
    val der = n.deref
    run(der.node, run(der.renaming, as))
  }
  
  def run(r: Renaming, as: List[Value]): List[Value] = {
    r.vector.map(i => if(i >= 0 && i < as.size) as(i) else Bottom)
  }
  
  def run(n: Node, as: List[Value]): Value = {
    val ress = n.outs.map(h => h.run(run(h.source.renaming.inv, as), run _))
    assert(ress.size == 1)
    ress.head
  }
}