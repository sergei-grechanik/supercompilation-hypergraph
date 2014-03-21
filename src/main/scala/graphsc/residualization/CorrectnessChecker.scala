package graphsc
package residualization

import scala.collection.immutable.Vector

object CorrectnessChecker {
  def globallySafe(lst: List[RSMatrix]): Boolean = {
    def go(lst: List[List[RelationState]]): Boolean = lst match {
      case Nil => true
      case hd :: _ if hd.isEmpty => true
      case _ =>
//        println("go:")
//        for(l <- lst)
//          println(l.mkString(" "))
        lst.zipWithIndex.find(p => p._1.foldLeft[RelationState](RSEq)(_ * _) == RSLess) match {
          case None => false
          case Some((rss,i)) =>
            val newlst =
              for((l,j) <- lst.zipWithIndex if i != j) yield
                for((RSEq,x) <- rss zip l) yield x
            go(newlst)
        }
    }
    go(lst.map(_.diag).transpose)
  }
  
  def renamingToMatrix(r: Renaming): RSMatrix = {
    val ar = r.arity
    RSMatrix(
      (RSEq +: Vector.fill(ar)(RSUnknown)) +:
        r.vector.map(i => 
          RSUnknown +: 
            (if(i == -1) Vector.fill(ar)(RSUnknown) 
             else (0 until ar).map(j => if(i == j) RSEq else RSUnknown).toVector)).toVector)
  }
  
  def buildMatrix(guard: RelationState, rsize: Int, asize: Int, 
                  f: (Int, Int) => RelationState): RSMatrix = {
    RSMatrix(
        (guard +: Vector.fill(asize)(RSUnknown)) +:
        (0 until rsize).map(i => RSUnknown +: (0 until asize).map(j => f(i,j)).toVector).toVector)
  }
  
  def idMatrix(r: RenamedNode): RSMatrix = 
    buildMatrix(RSEq, r.arity, r.arity, (i: Int,j: Int) => if(i == j) RSEq else RSUnknown)
  
  def hyperedgeToMatrix(h: Hyperedge): List[RSMatrix] = {
    val srcar = h.source.arity
    val eqfun = (i: Int,j: Int) => if(i == j) RSEq else RSUnknown
    val mainpart = h.label match {
      case Construct(_) =>
        h.dests.map(d => buildMatrix(RSLess, d.arity, srcar, eqfun))
      case CaseOf(cases) =>
        val varnum = h.dests(0).getVar
        buildMatrix(RSUnknown, h.dests(0).arity, srcar, eqfun) ::
          (for((d,(_,sh)) <- h.dests.tail zip cases) yield {
            buildMatrix(RSEq, d.arity, srcar, { (i,j) =>
              if(i < sh && varnum == Some(j)) RSLess
              else if(i >= sh && j == i - sh) RSEq
              else RSUnknown
            })
          })
      case Let() =>
        buildMatrix(RSEq, h.dests(0).arity, srcar, { (i,j) =>
          if(h.dests(i + 1).getVar == Some(j)) RSEq
          else RSUnknown
        }) ::
        h.dests.tail.map(d => buildMatrix(RSUnknown, d.arity, srcar, eqfun))
      case Tick() =>
        h.dests.map(d => buildMatrix(RSLess, d.arity, srcar, eqfun))
      case Improvement() =>
        h.dests.map(d => buildMatrix(RSLess, d.arity, srcar, eqfun))
      case Id() => 
        h.dests.map(d => buildMatrix(RSEq, d.arity, srcar, eqfun))
      case Var() =>
        Nil
      case Unused() =>
        Nil
    }
    val srcmat = renamingToMatrix(h.source.renaming.inv)
    (mainpart, h.dests).zipped.map((m,d) => renamingToMatrix(d.renaming) * m * srcmat)
  }
  
  def checkCallGraph(igraph: Set[(Int, Int, RSMatrix)]): Boolean = {
    val graph = collection.mutable.Set[(Int, Int, RSMatrix)]() ++ igraph
    var changed = true
    while(changed) {
      changed = false
      for((i1,j1,m1) <- graph; (i2,j2,m2) <- graph; if j1 == i2) {
        val edge = (i1, j2, m2 * m1)
        if(!graph.contains(edge)) {
          graph += edge
          changed = true
        }
      }
    }
    
//    println("after saturation:")
//    for((i,j,m) <- graph) {
//      println(i + " -> " + j)
//      for(row <- m.mat)
//        println(row.mkString(" "))
//    }
//    println("")
    
    graph.collect{ case (i,j,m) if i == j => (i, m) }.groupBy(_._1)
      .forall(x => globallySafe(x._2.toList.map(_._2)))
  }
}

sealed trait RelationState {
  def *(o: RelationState): RelationState = (this, o) match {
    case (RSUnknown, _) => RSUnknown
    case (_, RSUnknown) => RSUnknown
    case (RSLess, x) => RSLess
    case (x, RSLess) => RSLess
    case (RSEq, RSEq) => RSEq
  }
  
  def +(o: RelationState): RelationState = (this, o) match {
    case (RSUnknown, x) => x
    case (x, RSUnknown) => x
    case (RSLess, x) => RSLess
    case (x, RSLess) => RSLess
    case (RSEq, RSEq) => RSEq
  }
  
  override def toString: String = this match {
    case RSUnknown => "?"
    case RSLess => "<"
    case RSEq => "="
  }
}

case object RSUnknown extends RelationState
case object RSEq extends RelationState
case object RSLess extends RelationState

case class RSMatrix(mat: Vector[Vector[RelationState]]) {
  def *(prev: RSMatrix): RSMatrix = {
    val tr = prev.mat.transpose
    RSMatrix(
      (for(i <- 0 until mat.size) yield {
        for(j <- 0 until tr.size) yield
          (mat(i), tr(j)).zipped.map(_ * _).foldLeft[RelationState](RSUnknown)(_ + _)
      }.toVector).toVector)
  }
  
  def diag: List[RelationState] =
    (0 until mat.size).map(i => mat(i)(i)).toList
    
  def locallySafe: Boolean = diag.contains(RSLess)
  
  override def toString = mat.size + "x" + mat(0).size 
}

