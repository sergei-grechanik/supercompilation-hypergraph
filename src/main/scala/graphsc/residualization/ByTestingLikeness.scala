package graphsc
package residualization

import graphsc.interpretation._

class ByTestingLikenessCalculator(graph: HyperTester) extends LikenessCalculator(graph.total) {
  val lc = new LikenessCalculator(graph.total)
  
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
    val lvars = l.used.toList
    val rvars = r.used.toList
    
    val res =
      for(perm <- lvars.permutations; rn <- Renaming(rvars zip perm :_*) | ren;
          if consistent(l, r, rn))
        yield rn
        
    res.toList
  }
  
  override def viablePermutations(n: Node): List[Renaming] = {
    val res = viableRenamings(n, n)
    
    val other = lc.viablePermutations(n)
    if(other.toSet != res.toSet) {
      println("viablePermutations")
      println(n.prettyDebug)
      println("res = " + res)
      println("oth = " + other)
      println("")
      println(graph.runCache(n).map(x => x._1 + " -> " + x._2.value).mkString("\n"))
      println("")
    }
    
    res
  }
  
  override def likenessN(
        l: Node, r: Node, ren: Renaming = Renaming(),
        hist: List[(Node, Node)] = Nil): Option[(Int, Renaming)] = {
    val res =
      viableRenamings(l, r, ren) match {
        case Nil => None
        case lst => Some((graph.runCache(l).size min graph.runCache(r).size, lst.reduce(_ & _)))
      }
    
    val other = lc.likenessN(l, r, ren, hist)
    
    if(other.map(_._2) != res.map(_._2) && (res == None || 
        other.exists(o => res.exists(r => (o._2 | r._2) == None)))) {
      println("likenessN " + ren)
      println("")
      println(l.prettyDebug)
      println("")
      println(r.prettyDebug)
      println("")
      println("res = " + res)
      println("oth = " + other)
      println("")
      println(graph.runCache(l).map(x => x._1 + " -> " + x._2.value).mkString("\n"))
      println("")
      println(graph.runCache(r).map(x => x._1 + " -> " + x._2.value).mkString("\n"))
      println("")
    }
    
    res
  }  
}