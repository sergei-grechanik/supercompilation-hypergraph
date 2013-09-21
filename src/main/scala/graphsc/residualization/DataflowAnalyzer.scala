package graphsc.residualization

import graphsc._

trait DataflowFramework[T] {
  type Type = T
  def init(n: Node): T
  def top: T
  def meet(a: T, b: T): T
  def trans(h: Hyperedge, l: List[T]): T
}

// All this stuff can only be used if the hypergraph doesn't have vacuous cycles
case class DataflowAnalyzer[T](framework: DataflowFramework[T]) {
  import framework._
  
  def apply(nodes: Set[Node], outs: Node => Iterable[Hyperedge]): Map[Node, T] = {
    val map = 
      collection.mutable.Map[Node, T](nodes.toList.map(n => (n, init(n))):_*)
    var changed = true
    while(changed) {
      changed = false
      for(n <- map.keys) {
        val transed =
          for(h <- outs(n)) yield {
            val list = h.dests.map(n => map(n.node))
            trans(h, list)
          }
        
        val joined = (top /: transed)(meet)
        
//        println(n.prettyDebug)
//        println(n.outs.mkString("\n"))
//        println(transed)
//        println("before: " + map(n))
//        println("after: " + joined)
        
        if(joined != map(n)) {
          changed = true
//          println(n.prettyDebug)
//          println("before: " + map(n))
//          println("after: " + joined)
          map(n) = joined
        }
      }
    }
    
    map.toMap
  }
  
  def apply(graph: Hypergraph): Map[Node, T] =
    apply(graph.allNodes, (_.outs))
  
}

object DataflowAnalyzer {
  object ReturningConstrsFramework extends DataflowFramework[Option[Set[String]]] {
    override def top = None
    override def init(n: Node): Type = Some(Set())
    override def meet(a: Type, b: Type): Type = (a,b) match {
      case (None, _) => b
      case (_, None) => a
      case (Some(sa), Some(sb)) => Some(sa & sb)
    }
    def combine(a: Type, b: Type): Type =
      for(sa <- a; sb <- b) yield (sa | sb)
    override def trans(h: Hyperedge, l: List[Type]): Type = h.label match {
      case Construct(name) => Some(Set(name))
      case CaseOf(cases) =>
        val ctrs = l(0).getOrElse(cases.map(_._1).toSet)
        val res = (Option(Set[String]()) /: (cases zip l.tail).filter(p => ctrs(p._1._1)).map(_._2))(combine)
//        println(h.source.node.prettyDebug)
//        println(l)
//        println(ctrs)
//        println(res)
//        println("")
        res
      case Unused() => Some(Set())
      case Var() => None
      case Let() => l(0)
      case _ => l(0)
    }
  }
  
  def returningConstrs =
    DataflowAnalyzer(ReturningConstrsFramework)
}
