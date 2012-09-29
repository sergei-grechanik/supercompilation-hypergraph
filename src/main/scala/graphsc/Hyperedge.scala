package graphsc

case class Hyperedge(label: Label, source: RenamedNode, dests: List[RenamedNode]) {
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
    case Error() => Set()
  }
  
  def asDummyNode: RenamedNode =
    (new Node(used)).deref
  
  // Returns the same hyperedge but with different source
  def from(newsrc: RenamedNode): Hyperedge =
    Hyperedge(label, newsrc, dests)
    
  def freeSource: Hyperedge =
   from((new FreeNode(used)).deref)
  
  // Replace a node in source and destination nodes
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = 
      if(source.node == old) 
        RenamedNode(source.renaming, n)
      else 
        source
        
    val newdst = 
      for(d <- dests)
        yield if(d.node == old) RenamedNode(d.renaming, n) else d
        
    Hyperedge(label, newsrc, newdst)
  }
  
  // Dereference all glued nodes
  def deref: Hyperedge =
    Hyperedge(label, source.deref, dests.map(_.deref))
  
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
      case Error() =>
        Bottom
    }
  }
}
