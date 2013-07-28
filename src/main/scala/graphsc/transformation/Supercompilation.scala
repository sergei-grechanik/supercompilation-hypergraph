package graphsc
package transformation

class Supercompilation(graph: Transformations, maxdepth: Int = 10) {
  def supercompile(n: Node) {
    val visited = collection.mutable.Set[Node]()
    
    def go(n: Node, hist: List[Node]) {
      if(!visited.exists(_ ~~ n)) {
        visited += n
        for(h <- makeSteps(n, hist, visited.toSet); d <- h.dests)
          go(d.node, n :: hist)
      } 
    }
    
    go(n, Nil)
  }
  
  def makeSteps(n: Node, hist: List[Node], visited: Set[Node]): List[Hyperedge] = {
    if(whistle(n, hist, visited))
      generalizationSteps(n, hist)
    else
      drivingSteps(n)
  }
  
  def whistle(n: Node, hist: List[Node], visited: Set[Node]): Boolean =
    hist.size > maxdepth
  
  def drivingSteps(n: Node): List[Hyperedge] =
    graph.drive(n).toList
    
  def generalizationSteps(n: Node, hist: List[Node]): List[Hyperedge] =
    graph.drive(n).toList
}
