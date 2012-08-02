package graphsc

case class Value(constructor: String, args: List[Value]) {
  override def toString = constructor + " " + args.map("(" + _ + ")").mkString(" ")
}

sealed trait Label {
  def bound(destnum: Int): Set[Int] = Set()
}

case class Construct(name: String)
  extends Label

case class CaseOf(cases: List[(String, List[Int])])
  extends Label {
  override def bound(destnum: Int): Set[Int] = 
    if(destnum >= 1 && destnum <= cases.length)
      cases(destnum - 1)._2.toSet
    else
      Set()
}

case class Let(variable: Int)
  extends Label {
  require(variable >= 0)
  
  override def bound(destnum: Int): Set[Int] =
    if(destnum == 0)
      Set(variable)
    else
      Set()
}

case class Tick
  extends Label

// Should be a permutation
case class Renaming(vector: Vector[Int])
  extends Label {
  require(vector.toSet == (0 until vector.length).toSet)
  
  def comp(other: Renaming): Renaming = {
    val w = arity max other.arity
    val t1 = this.widen(w)
    val o1 = other.widen(w)
    Renaming(o1.vector.map(t1(_)))
  }
  
  def inv: Renaming = {
    val m = Map(vector.zipWithIndex : _*)
    Renaming(Vector() ++ (0 to m.keys.max).map(i => m.getOrElse(i, -1)))
  }
  
  def isId: Boolean =
    vector.zipWithIndex.forall{case (a,b) => a == b}
  
  def this(arity: Int) = this(Vector() ++ (0 until arity))
  def this(p: (Int, Int)) = 
    this(Vector() ++ (0 to (p._1 max p._2)).toArray.map(i => 
      if(i == p._1) p._2 else if(i == p._2) p._1 else i ))
  def this(i: Iterable[Int]) = this(Vector() ++ i)
  
  def arity: Int = vector.max + 1
  
  def widen(a: Int): Renaming =
    Renaming(Vector() ++ (0 until a).map(i => if(i < vector.length) vector(i) else i))
  
  def apply(i: Int): Int = vector(i)
}

case class Var
  extends Label

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  label match {
    case r: Renaming =>
      require(r.arity >= dests(0).arity)
    case _ =>
  }
  require(source == null || source.arity == arity)
  
  def arity: Int = label match {
    case r: Renaming => r.arity
    case v: Var => 1
    case _ =>
      if(dests.nonEmpty)
        dests.map(_.arity).max
      else
        0
  }
  
  def from(newsrc: Node): Hyperedge =
    Hyperedge(label, newsrc, dests)
  
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = if(source == old) n else source
    val newdst = 
      for(d <- dests)
        yield if(d == old) n else d
    Hyperedge(label, newsrc, newdst)
  }
  
  def derefGlued: Hyperedge = {
    val s = if(source == null) null else source.getRealNode
    Hyperedge(label, s, dests.map(_.getRealNode))
  }
      
  
  // Why do I use lists here? mb replace with vectors?
  def run(args: List[Value], nodeRunner: (Node, List[Value]) => Value): Value = label match {
    case Construct(name) =>
      Value(name, dests.map(nodeRunner(_, args)))
    case CaseOf(cases) =>
      val victim = nodeRunner(dests(0), args)
      val Some(((_, vars), expr)) = (cases zip dests.tail).find(_._1._1 == victim.constructor)
      val newargs =
        for((v,i) <- args.zipWithIndex) yield {
          val j = vars.indexWhere(_ == i)
          if(j >= 0)
            victim.args(j)
          else
            v
        }
      nodeRunner(expr, newargs)
    case Let(x) =>
      // Well, it's not lazy yet, but I don't know how to do laziness right in scala
      val newargs =
        for((v,i) <- args.zipWithIndex) yield {
          if(i == x)
            nodeRunner(dests(1), args)
          else
            v
        }
      nodeRunner(dests(0), newargs)
    case Tick() =>
      nodeRunner(dests(0), args)
    case r@Renaming(_) =>
      val newargs = (0 until dests(0).arity).map(i => args(r(i))).toList
      nodeRunner(dests(0), newargs)
    case Var() => args(0)
  }
}

class Node(val arity: Int) {
  private val mouts = collection.mutable.Set[Hyperedge]()
  private val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: Node = null
  
  def outs: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else getRealNode.outs
  def ins: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else getRealNode.ins
  
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def getRealNode: Node =
    if(gluedTo == null) this
    else gluedTo.getRealNode
}
