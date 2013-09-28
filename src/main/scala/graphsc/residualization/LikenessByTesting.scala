package graphsc
package residualization

import graphsc.interpretation._

class ByTestingLikenessCalculator(graph: HyperTester) 
    extends DefaultLikenessCalculator(graph.total) {
  val lc = new DefaultLikenessCalculator(graph.total)
  
  def consistent(l: Node, r: Node, ren: Renaming): Boolean = {
    graph.runCache(l).forall{
      case (as, res) =>
        val bs = ren.vector.map(i => if(i >= 0 && i < as.size) as(i) else Bottom)
        graph.runCache(r).get(bs).fold(true)(_.value == res.value)
    } &&
    graph.runCache(r).forall{
      case (as, res) =>
        val bs = ren.inv.vector.map(i => if(i >= 0 && i < as.size) as(i) else Bottom)
        graph.runCache(l).get(bs).fold(true)(_.value == res.value)
    }
  }
  
  def viableRenamings(l: Node, r: Node, ren: Renaming = Renaming()): List[Renaming] = {
    if(l.used.size > r.used.size)
      viableRenamings(r, l, ren.inv).map(_.inv)
    else {
      val rvars = r.used.toList
      val lvars1 = l.used.toList
      val lvars = lvars1 ++ List.fill(rvars.size - lvars1.size)(-1)
      
      val res =
        for(perm <- lvars.permutations; 
            newren = Renaming(rvars zip perm :_*);  _ <- newren | ren;
            if consistent(l, r, newren))
          yield newren

      res.toList
    }
  }
  
  override def viablePermutations(n: Node): List[Renaming] = {
    val res = viableRenamings(n, n)
    val other = lc.viablePermutations(n)
    (res.toSet & other.toSet).toList
  }
  
  override def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] = {
    lc.likenessN(l, r, ren, hist).flatMap {
      case (i, ren1) =>
        viableRenamings(l, r, ren1) match {
          case Nil => None
          case lst => 
            Some((i*100 + (graph.runCache(l).size min graph.runCache(r).size), lst.reduce(_ & _)))
        }
    }
  }  
  
}