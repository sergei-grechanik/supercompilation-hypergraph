package graphsc

// Should be injective, i.e. no gluing of variables
case class Renaming(vector: List[Int]) {
  // This check is very bad for performance
  //require(vector.filter(_ >= 0).distinct.size == vector.filter(_ >= 0).size)
  //require(vector.forall(_ >= -1))
  
  override def toString: String =
    vector.zipWithIndex.map{case (j,i) => i + " = " + j}.mkString("(", ", ", ")")
  
  def toMap: Map[Int, Int] =
    vector.zipWithIndex.filter(_._1 != -1).map(_.swap).toMap
  
  def codomain: Set[Int] =
    vector.filter(_ != -1).toSet
    
  def domain: Set[Int] =
    vector.zipWithIndex.filter(_._1 != -1).map(_._2).toSet
    
  def arity: Int =
    (-1 :: vector).max + 1
    
  def isId(used: Set[Int]) =
    vector.zipWithIndex.forall{ case (a,b) => !used(b) || a == b }
    
  def seemsId =
    vector.zipWithIndex.forall{ case (a,b) => a == -1 || a == b }
    
  def isPermutation =
    vector.sorted == (0 until vector.length).toList
  
  def apply(i: Int): Int =
    if(i >= 0 && i < vector.size)
      vector(i)
    else
      -1
      
  def normal: Renaming =
    Renaming(vector.reverse.dropWhile(_ == -1).reverse)
    
  def inv: Renaming =
    Renaming(vector.zipWithIndex.filter(_._1 != -1) : _*)
    
  def comp(r: Renaming): Renaming =
    Renaming(r.vector.map(this(_))).normal
  
  def comp(r: RenamedNode): RenamedNode =
    RenamedNode(this comp r.renaming, r.node)
  
  def comp(n: Node): RenamedNode =
    RenamedNode(this, n).normal
    
  def comp(used: Set[Int]): Set[Int] =
    used.map(this(_)) - (-1)
    
  def compDests(h: Hyperedge): List[RenamedNode] = h.label match {
    case Id() => List(this comp h.dests(0))
    case Tick() => List(this comp h.dests(0))
    case Improvement() => List(this comp h.dests(0))
    case Var() =>
      if(this(0) == 0) h.dests
      else throw new RuntimeException("Composing a renaming with a Var is not a good idea")
    case Construct(_) =>  h.dests.map(this comp _)
    case Let() => 
      h.dests(0) :: h.dests.tail.map(this comp _)
    case CaseOf(cases) =>
      val newcasedests = 
        h.dests.tail.zip(cases).map {
          case (d,(_,n)) => this.shift(n) comp d
        }
      (this comp h.dests(0)) :: newcasedests
    case Unused() =>
      Nil
  }
  
  def comp(h: Hyperedge): Hyperedge = {
    Hyperedge(h.label, this comp h.source, this compDests h)
  }
  
  def mapVars(f: Int => Int): Renaming =
    Renaming(vector.map(i => if(i == -1) -1 else f(i))).normal
  
  def shift(n: Int): Renaming = {
    // The zeroth dest of a Let has a shift -1 because there is no shifting in let, outer
    // arguments are discarded
    require(n >= 0)
    Renaming((0 until n toList) ++ vector.map(i => if(i == -1) -1 else i + n))
  }

  def unshift(n: Int): Renaming = {
    require(n >= 0)
    Renaming(vector.drop(n).map(i => 
      if(i == -1) -1 
      else if(i >= n) i - n
      else -1))
  }
        
  // combine two renamings. None if there is a contradiction 
  // (i.e. if they map some variables to different variables)
  def |(r: Renaming): Option[Renaming] = {
    val v1 = vector.zipWithIndex.filter(_._1 != -1).map(_.swap)
    val v2 = r.vector.zipWithIndex.filter(_._1 != -1).map(_.swap)
    val map = (v1 ++ v2).distinct.groupBy(_._1)
    val comap = (v1 ++ v2).distinct.groupBy(_._2)
    if(map.forall(_._2.size == 1) && comap.forall(_._2.size == 1))
      Some(Renaming(map.iterator.map(_._2.head).toSeq: _*))
    else
      None
  }
}

object Renaming {
  def apply(pairs: (Int, Int)*): Renaming = 
    if(pairs.isEmpty)
      Renaming(Nil)
    else {
      val map = pairs.toMap
      require(map.size == pairs.size)
      Renaming((0 to map.keys.max toList).map(map.getOrElse(_, -1))).normal
    }
  def apply(used: Set[Int]): Renaming =
    apply(used.toList.map(x => (x,x)):_*)
}

// theta . node
case class RenamedNode(renaming: Renaming, node: Node) {
  def isPlain = renaming.isId(node.used)
  def plain = RenamedNode.fromNode(node)
  def arity: Int = (used + (-1)).max + 1
  def used: Set[Int] = renaming comp node.used
  def deref = (renaming comp node.deref).normal
  def normal =
    RenamedNode(
        Renaming((0 until node.arity toList).map(i => 
          if(node.used(i)) renaming(i) else -1)).normal, 
        node)
        
  def isInvertible =
    node.used.size == used.size
        
  def getVar: Option[Int] =
    if(node.isVar)
      Some(renaming(0))
    else
      None
      
  def getVarUnused: Option[Int] =
    getVar match {
      case Some(i) => Some(i)
      case None =>
        node.definingHyperedge match {
          case Some(h) if h.label == Unused() => Some(-1)
          case _ => None
        }
    }
   
  // Resturns true if the nodes are equal up to renaming
  def ~~(n: RenamedNode): Boolean = node ~~ n.node
      
  // Assign numbers to the variables used by node but marked unused by renaming
  def restoreUnused(fromvar: Int): (Int, RenamedNode) = {
    var curvar = fromvar
    val u = node.used
    val newvec =
      (0 until (renaming.vector.size max ((node.used + (-1)).max + 1))).map {
        case j =>
          val i = renaming(j)
          if(i != -1) i
          else if(u(j)) {
            curvar += 1
            curvar
          }
          else -1
      }
    (curvar, Renaming(newvec.toList) comp node)
  }
}

object RenamedNode {
  def fromNode(node: Node): RenamedNode = 
    RenamedNode(Renaming(0 until node.arity toList), node).normal
}