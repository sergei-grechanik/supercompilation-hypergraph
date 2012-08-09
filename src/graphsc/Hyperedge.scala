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

case class Let(vars: List[Int])
  extends Label {
  require(vars.forall(_ >= 0) && vars.toSet.size == vars.size)
  
  override def bound(destnum: Int): Set[Int] =
    if(destnum == 0)
      vars.toSet
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
    
  // Reduce the domain of the renaming to this set of variables
  // used to equalize more renamings
  def reduce(used: Set[Int]): Renaming =
    Renaming(vector.zipWithIndex.filter(p => used(p._1)).toMap)
    
  // Move the renaming up through a binding, i.e. find r1 such that
  // this . shift(bnd) = r1 . shift(this^-1(bnd))
  def upThroughBinding(bnd: List[Int]): (Renaming, List[Int]) = {
    import Renaming._
    val bndset = bnd.toSet
    val thisinv = this.inv
    val newbnd = bnd.map(thisinv(_))
    val newrnm = Renaming(Vector() ++
        (0 to ((bndset.size max (vector.length - 1)) - bndset.size) map {i =>
          shift(bndset)(this(shiftInv(newbnd.toSet)(i))) 
      })).normalize
    (newrnm, newbnd)
  }
}

object Renaming {
  def apply(): Renaming = Renaming(Vector())
  
  def apply(p: (Int, Int)): Renaming = 
    Renaming(Vector() ++ (0 to (p._1 max p._2)).map(i => 
      if(i == p._1) p._2 else if(i == p._2) p._1 else i )).normalize
  
  // m should be one-to-one
  def apply(m: Map[Int, Int]): Renaming = {
    if(m.isEmpty)
      return Renaming()
      
    val maxvar = m.keys.max max m.values.max
    val free = ((0 to maxvar).toSet &~ m.values.toSet).toSeq.sorted
    var freej = -1
    val vec = Vector() ++
      (0 to maxvar map { i =>
        m.getOrElse(i, {freej += 1; free(freej)}) 
      })
    Renaming(vec).normalize
  }
      
  def shift(bnd: Set[Int])(i: Int): Int = {
    require(!bnd(i) && i >= 0)
    i - bnd.count(_ < i)
  }
  
  def shiftInv(bnd: Set[Int])(i: Int): Int = {
    require(i >= 0)
    var j = -1
    var sj = -1
    while(sj != i) {
      j += 1
      if(!bnd(j))
        sj += 1
    }
    assert(shift(bnd)(j) == i)
    j
  }
  
  // This renaming takes variable numbers before binding
  // to the corresponding variables after binding.
  def shiftInvRenaming(bnd: Set[Int], varcount: Int): Renaming = {
    var j: Int = -1
    val pairs =
      0 until varcount map { i =>
        j += 1
        while(bnd(j)) j += 1
        assert(shift(bnd)(j) == i)
        (i,j)
      }
    Renaming(pairs.toMap)
  }
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
        for((d,i) <- dests.zipWithIndex) yield {
          val bnd = label.bound(i)
          (d.used &~ bnd).map(Renaming.shift(bnd) _)
        }
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
  
  /*def run(args: Vector[Value], runNode: (Node, Vector[Value]) => Value): Value = {
    println("running " + this + " on " + args)
    val res = run1(args, runNode)
    println("run " + this)
    println(args + " -> " + res)
    res
  }*/
    
  def run(args: Vector[Value], runNode: (Node, Vector[Value]) => Value): Value = {
    def nodeRunner(n: Node, args: IndexedSeq[Value]): Value = {
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
        val shiftfun = Renaming.shift(vars.toSet) _
        val newargs =
          for(i <- 0 until expr.varCount) yield {
            val j = vars.indexWhere(_ == i)
            if(j != -1)
              victim.args(j)
            else
              args(shiftfun(i))
          }
        nodeRunner(expr, newargs)
      case Let(vars) =>
        // Well, it's not lazy yet, but I don't know how to do laziness right in scala
        val shiftfun = Renaming.shift(vars.toSet) _
        val newargs =
          for(i <- 0 until dests(0).varCount) yield {
            val j = vars.indexWhere(_ == i)
            if(j != -1)
              nodeRunner(dests(j + 1), args)
            else
              args(shiftfun(i))
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
  
  def varCount: Int =
    if(used.isEmpty) 0 else used.max + 1
  
  def used: Set[Int] = 
    if(gluedTo == null) mused else getRealNode.used
  def outs: Set[Hyperedge] = 
    if(gluedTo == null) mouts.toSet else getRealNode.outs
  def ins: Set[Hyperedge] = 
    if(gluedTo == null) mins.toSet else getRealNode.ins
  
  def outsMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else getRealNode.outsMut
  def insMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else getRealNode.insMut
    
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

object Hyperedge {
  // Create a new auxiliary node for this hyperedge
  def apply(l: Label, dst: List[Node]): Hyperedge = {
    val h = Hyperedge(l, null, dst)
    val n = new Node(h.used)
    val res = h.from(n)
    n.outsMut += res
    res
  }
}

object Node {
  // Create an auxiliary node with one hyperedge
  def apply(l: Label, dst: List[Node]): Node = 
    Hyperedge(l, dst).source
  
  def apply(h: Hyperedge): Node = {
    if(h.source == null) {
      apply(h.label, h.dests)
    } else {
      h.source.outsMut += h
      h.source
    }
  }
}
