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
case class Renaming private(vector: Vector[Int])
  extends Label {
  require(vector.toSet == (0 until vector.length).toSet)
  
  override def toString =
    "Renaming(" + vector.mkString(" ") + ")"
  
  def comp(other: Renaming): Renaming =
    Renaming(Vector() ++ 
        (0 until (vector.length max other.vector.length)).map
          (i => this(other(i)))).normalize
  
  def inv: Renaming = {
    val m = Map(vector.zipWithIndex : _*)
    Renaming(Vector() ++ (0 until vector.length).map(i => m(i))).normalize
  }
  
  def isId: Boolean =
    vector.length == 0
  
  def normalize: Renaming =
    Renaming(Vector() ++ 
        vector.zipWithIndex.reverse.dropWhile{case (a,b) => a == b}.map(_._1).reverse)
    
  def apply(i: Int): Int = 
    if(i < vector.length) vector(i) else i
}

object Renaming {
  def apply(): Renaming = Renaming(Vector())
  
  def apply(p: (Int, Int)): Renaming = 
    Renaming(Vector() ++ (0 to (p._1 max p._2)).toArray.map(i => 
      if(i == p._1) p._2 else if(i == p._2) p._1 else i )).normalize
}

case class Var
  extends Label

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  require(source == null || used.subsetOf(source.used))
  
  // Set of used variables
  def used: Set[Int] = label match {
    case Var() => Set(0)
    case r: Renaming => dests(0).used.map(r(_))
    case _ =>
      // remove bound variables from the dests' used sets
      val destused =
        for((d,i) <- dests.zipWithIndex) yield
          d.used &~ label.bound(i) 
      (Set[Int]() /: destused)(_ | _)
  } 
  
  // Returns the same hyperedge but with different source
  def from(newsrc: Node): Hyperedge =
    Hyperedge(label, newsrc, dests)
  
  // Replace a node in source and destination nodes
  def replace(old: Node, n: Node): Hyperedge = {
    val newsrc = if(source == old) n else source
    val newdst = 
      for(d <- dests)
        yield if(d == old) n else d
    Hyperedge(label, newsrc, newdst)
  }
  
  // Dereference all glued nodes
  def derefGlued: Hyperedge = {
    val s = if(source == null) null else source.getRealNode
    Hyperedge(label, s, dests.map(_.getRealNode))
  }
  
  override def toString =
    source.uniqueName + " -> " + label + " -> " + dests.map(_.uniqueName).mkString(" ")
      
  def run(args: Vector[Value], runNode: (Node, Vector[Value]) => Value): Value = {
    def nodeRunner(n: Node, args: Vector[Value]): Value = {
      // we replace unused variables with nulls to 
      // normalize our argument list
      // should we move this code to HyperTester?
      val newargs =
        if(n.used.nonEmpty)
          Vector() ++
          (0 to n.used.max).map { i =>
            if(n.used(i)) {
              require(i < args.length && args(i) != null)
              args(i)
            } else
              null
          }
        else
          Vector()
      runNode(n, newargs)
    }
    
    label match {
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
      case r: Renaming =>
        val newargs = Vector() ++
          (0 until (args.length max r.vector.length)).map { i => 
            if(r(i) < args.length) args(r(i)) else null
          }
        nodeRunner(dests(0), newargs)
      case Var() => args(0)
    }
  }
}

class Node(var mused: Set[Int]) {
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: Node = null
  
  def used: Set[Int] = 
    if(gluedTo == null) mused else getRealNode.used
  def outs: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else getRealNode.outs
  def ins: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else getRealNode.ins
  
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def getRealNode: Node =
    if(gluedTo == null) this
    else gluedTo.getRealNode
    
  def uniqueName: String =
    if(gluedTo == null)
      super.toString
    else
      super.toString + "(" + getRealNode.uniqueName + ")"
  
  override def toString =
    uniqueName +
    (if(gluedTo != null) 
      " gluedTo\n" + gluedTo.toString
     else
      "\n\nuses: " + used.mkString(" ") +
      "\n\nouts:\n" + outs.mkString("\n") +
      "\n\nins:\n" + ins.mkString("\n"))
}
