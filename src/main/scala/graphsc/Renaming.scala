package graphsc

// Should be injective, i.e. no gluing of variables
case class Renaming(vector: List[Int]) {
  require(vector.filter(_ >= 0).distinct.size == vector.filter(_ >= 0).size)
  
  override def toString: String =
    vector.zipWithIndex.map{case (j,i) => i + " = " + j}.mkString("(", ", ", ")")
  
  def isId(used: Set[Int]) =
    vector.zipWithIndex.forall{ case (a,b) => !used(b) || a == b }
  
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
    
  def compDests(h: Hyperedge): Hyperedge = h.label match {
    case Id() => Hyperedge(h.label, h.source, List(this comp h.dests(0)))
    case Tick() => Hyperedge(h.label, h.source, List(this comp h.dests(0)))
    case Improvement() => Hyperedge(h.label, h.source, List(this comp h.dests(0)))
    case Var() =>
      if(this(0) == 0) h
      else throw new RuntimeException("Composing a renaming with a Var is not a good idea")
    case Construct(_) => Hyperedge(h.label, h.source, h.dests.map(this comp _))
    case Let() => 
      Hyperedge(h.label, h.source, h.dests(0) :: h.dests.tail.map(this comp _))
    case CaseOf(cases) =>
      val newcasedests = 
        h.dests.tail.zip(cases).map {
          case (d,(_,n)) => this.throughShift(n) comp d
        }
      Hyperedge(h.label, h.source, (this comp h.dests(0)) :: newcasedests)
    case Error() =>
      Hyperedge(h.label, h.source, Nil)
  }
  
  def comp(h: Hyperedge): Hyperedge = {
    val Hyperedge(l, s, d) = this compDests h
    Hyperedge(l, this comp s, d)
  }
  
  def throughShift(n: Int): Renaming =
    Renaming((0 until n toList) ++ vector.map(i => if(i == -1) -1 else i + n)) 
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
    if(node.outs.exists(_.label == Var()))
      Some(renaming(0))
    else
      None
      
  def getVarErr: Option[Int] =
    if(node.outs.exists(_.label == Var()))
      Some(renaming(0))
    else if(node.outs.exists(_.label == Error()))
      Some(-1)
    else
      None
}

object RenamedNode {
  def fromNode(node: Node): RenamedNode = 
    RenamedNode(Renaming(0 until node.arity toList), node).normal
}