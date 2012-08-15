package graphsc

case class Value(constructor: String, args: List[Value]) {
  override def toString = constructor + " " + args.map("(" + _ + ")").mkString(" ")
}

sealed trait Label {
  def isSimple: Boolean = this match {
    case Id() => true
    case Tick() => true
    case Improvement() => true
    case Construct(_) => true
    case _ => false
  }
}

case class Construct(name: String)
  extends Label

case class CaseOf(cases: List[(String, Int)])
  extends Label

case class Let(arity: Int)
  extends Label

// Like Let, but binds variables only to variables
// should be injective, i.e. no gluing of variables
case class Renaming(arity: Int, vector: List[Int])
  extends Label {
  require(vector.forall(_ < arity) && vector.distinct == vector)
}
  
case class Id
  extends Label
  
case class Tick
  extends Label
  
case class Improvement
  extends Label

case class Var(arity: Int, num: Int)
  extends Label {
  require(num < arity && num >= 0)
}

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  require(source == null || arity == source.arity)
  
  def arity: Int = label match {
    case Var(a, _) => a
    case Let(ar) =>
      assert(dests(0).arity == dests.tail.size)
      assert(dests.tail.forall(_.arity == ar))
      ar
    case Renaming(ar, vec) =>
      assert(dests(0).arity == vec.size)
      ar
    case CaseOf(cases) =>
      assert(cases.size == dests.size - 1)
      val ar = dests(0).arity
      assert(cases zip dests.tail forall { case ((_,n),d) => d.arity == ar + n })
      ar
    case _ =>
      dests match {
        case Nil => 0
        case d :: ds =>
          val ar = d.arity
          assert(ds.forall(_.arity == ar))
          ar
      }
  }
  
  def isId: Boolean = label match {
    case Id() => true
    case Let(ar) =>
      ar == dests.tail.length &&
      dests.tail.zipWithIndex.forall { case (d,i) =>
        d.outs.exists(h => h.label match {
          case Var(_, j) if i == j => true
          case _ => false
        })
      }
    case Renaming(ar, vec) => false //???
      //ar == vec.length &&
      //vec.zipWithIndex.forall{ case (a,b) => a == b }
    case _ => false
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
    
  def run(args: Vector[Value], nodeRunner: (Node, Vector[Value]) => Value): Value = {
    label match {
      case Construct(name) =>
        Value(name, dests.map(nodeRunner(_, args)))
      case CaseOf(cases) =>
        val victim = nodeRunner(dests(0), args)
        val Some(((_, n), expr)) = (cases zip dests.tail).find(_._1._1 == victim.constructor)
        assert(victim.args.size == n)
        nodeRunner(expr, args ++ victim.args)
      case Let(_) =>
        // Well, it's not lazy yet, but I don't know how to do laziness right in scala
        nodeRunner(dests(0), Vector() ++ dests.tail.map(nodeRunner(_, args)))
      case Renaming(_, vec) =>
        nodeRunner(dests(0), Vector() ++ vec.map(args(_)))
      case Tick() =>
        nodeRunner(dests(0), args)
      case Improvement() =>
        nodeRunner(dests(0), args)
      case Id() => 
        nodeRunner(dests(0), args)
      case Var(ar, i) =>
        assert(args.length == ar)
        args(i)
    }
  }
}

class Node(val arity: Int) {
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: Node = null
  
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
    uniqueName + "/" + arity +
    (if(gluedTo != null) 
      " gluedTo\n" + gluedTo.toString
     else
      "\n\narity: " + arity +
      "\n\nouts:\n" + outs.mkString("\n") +
      "\n\nins:\n" + ins.mkString("\n"))
}

object Hyperedge {
  // Create a new auxiliary node for this hyperedge
  def apply(l: Label, dst: List[Node]): Hyperedge = {
    val h = Hyperedge(l, null, dst)
    val n = new Node(h.arity)
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
