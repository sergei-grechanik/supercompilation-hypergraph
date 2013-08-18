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
    if(l.used.size > r.used.size)
      viableRenamings(r, l, ren.inv).map(_.inv)
    else {
      val rvars = r.used.toList
      val lvars1 = l.used.toList
      val lvars = lvars1 ++ List.fill(rvars.size - lvars1.size)(-1)
      
      val res =
        for(perm <- lvars.permutations; rn <- Renaming(rvars zip perm :_*) | ren;
            if consistent(l, r, rn))
          yield rn
          
      res.toList
    }
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
    //lc.likenessN(l, r, ren, hist).flatMap {
    //  case (i, ren1) =>
        viableRenamings(l, r, ren) match {
          case Nil => None
          case lst => 
            Some((1 + 0*100 + (graph.runCache(l).size min graph.runCache(r).size), lst.reduce(_ & _)))
        }
    //}
  }  
}