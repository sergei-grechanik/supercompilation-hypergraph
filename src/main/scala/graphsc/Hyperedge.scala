package graphsc

case class Hyperedge(label: Label, source: RenamedNode, dests: List[RenamedNode]) {
  // if source is not invertible then this hyperedge reduces its used set
  // if dests are not invertible then they won't consume some variables
  // require(dests.forall(_.isInvertible))
  
  // it is possible that dests ignore some variables that are used by the source
  // (in this case the used set of the source should be reduced)
  // but dests cannot use variables that are not declared used by the source
  // but we cannot test it here
  // require(source == null || used.subsetOf(source.used))
  
  label match {
    case _:Id => require(dests.size == 1)
    case _:Tick => require(dests.size == 1)
    case _:Improvement => require(dests.size == 1)
    //case Renaming(vec) => require(dests.size == 1 && dests(0).arity <= vec.size)
    case _:Var => require(dests.size == 0)
    //case _:Let => require(dests.size >= 1 && dests(0).arity <= dests.size - 1)
    case CaseOf(cases) => require(cases.size == dests.size - 1)
    case _ =>
  }
  
  def arity: Int = (used + (-1)).max + 1
  
  def used: Set[Int] = label match {
    case Id() => dests(0).used
    case Tick() => dests(0).used
    case Improvement() => dests(0).used
    case Var() => Set(0)
    case Construct(_) => (Set[Int]() /: dests.map(_.used))(_ | _)
    case Let() => 
      (Set[Int]() /: dests(0).used.collect { 
          case i if i < dests.tail.size => dests.tail(i).used
        })(_ | _)
    case CaseOf(cases) =>
      (dests(0).used /: (dests.tail zip cases).map{ 
          case (d,(_,n)) => d.used.map(_ - n).filter(_ >= 0) })(_ | _)
    case Unused() => Set()
  }
  
  def shifts: List[Int] = label match {
    case CaseOf(cases) => 0 :: cases.map(_._2)
    case Let() => (-1) :: dests.tail.map(_ => 0)
    case _ => dests.map(_ => 0)
  }
  
  def asDummyNode: RenamedNode =
    (new Node(used)).deref
  
  // Returns the same hyperedge but with different source
  def from(newsrc: RenamedNode): Hyperedge =
    Hyperedge(label, newsrc, dests)
    
  def freeSource: Hyperedge =
   from((new FreeNode(used)).deref)
  
  // Replace a node in source and destination nodes
  def replace(old: Node, n: RenamedNode): Hyperedge = {
    val newsrc = 
      if(source.node == old) 
        source.renaming comp n
      else 
        source
        
    val newdst = 
      for(d <- dests)
        yield if(d.node == old) d.renaming comp n else d
        
    Hyperedge(label, newsrc, newdst)
  }
  
  // Dereference all glued nodes
  def deref: Hyperedge =
    Hyperedge(label, source.deref, dests.map(_.deref))
  
  // Make dests' renamings non-invertible but consistent with the source's used set
  def reduceDestRenamings: Hyperedge =
    Renaming(source.used) comp this
    
  override def toString =
    source + " -> " + label + " -> " + dests.mkString(" ")
    
  def run(args: List[Value], nodeRunner: (RenamedNode, List[Value]) => Value): Value = {
    label match {
      case Construct(name) =>
        // Bottoms are like ordinary bottoms, ErrorBottoms propagate through constructors
        def subs = dests.map(nodeRunner(_, args))
        if(subs.contains(ErrorBottom))
          ErrorBottom
        else
          Ctr(name, subs)
      case CaseOf(cases) =>
        nodeRunner(dests(0), args) match {
          case Ctr(cname, cargs) =>
            val Some(((_, n), expr)) = (cases zip dests.tail).find(_._1._1 == cname)
            assert(cargs.size == n)
            nodeRunner(expr, cargs ++ args)
          case Bottom => Bottom
          case ErrorBottom => ErrorBottom
        }
      case Let() =>
        val newargs = dests.tail.map(nodeRunner(_, args))
        nodeRunner(dests(0), newargs)
      case Tick() =>
        nodeRunner(dests(0), args)
      case Improvement() =>
        nodeRunner(dests(0), args)
      case Id() => 
        nodeRunner(dests(0), args)
      case Var() =>
        args(0)
      case Unused() =>
        Bottom
    }
  }
}
