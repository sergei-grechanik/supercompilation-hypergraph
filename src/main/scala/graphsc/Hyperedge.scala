package graphsc
import sun.reflect.generics.reflectiveObjects.NotImplementedException

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
  
  override def toString: String =
    "Renaming/" + arity + "(" + vector.zipWithIndex.map{case (j,i) => i + " = " + j}.mkString(", ") + ")"
}
  
case class Id()
  extends Label
  
case class Tick()
  extends Label
  
case class Improvement()
  extends Label

case class Var(arity: Int, num: Int)
  extends Label {
  require(num < arity && num >= 0)
}

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
  require(source == null || arity == source.arity)
  label match {
    case _:Id => require(dests.size == 1)
    case _:Tick => require(dests.size == 1)
    case _:Improvement => require(dests.size == 1)
    case _:Renaming => require(dests.size == 1)
    case _:Var => require(dests.size == 0)
    case _ =>
  }
  
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
    
  def freeSource: Hyperedge =
   from(new FreeNode(arity))
  
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
    val s = if(source == null) null else source.realNode
    Hyperedge(label, s, dests.map(_.realNode))
  }
  
  override def toString =
    source + " -> " + label + " -> " + dests.mkString(" ")
  
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
            nodeRunner(expr, args ++ cargs)
          case Bottom => Bottom
          case ErrorBottom => ErrorBottom
        }
      case Let(_) =>
        val newargs = Vector() ++ dests.tail.map(nodeRunner(_, args))
        nodeRunner(dests(0), newargs)
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
  
  var prettyDebug = ""
  
  def outs: Set[Hyperedge] = 
    if(gluedTo == null) mouts.toSet else realNode.outs
  def ins: Set[Hyperedge] = 
    if(gluedTo == null) mins.toSet else realNode.ins
  
  def outsMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else realNode.outsMut
  def insMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else realNode.insMut
    
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def realNode: Node =
    if(gluedTo == null) this
    else gluedTo.realNode
    
  def uniqueName: String =
    if(gluedTo == null)
      super.toString + "/" + arity
    else
      super.toString + "/" + arity + "(" + realNode.uniqueName + ")"
  
  override def toString =
    uniqueName
}

class InvalidFreeNodeUseException extends 
  Exception("FreeNode should be glued and then dereferenced to be used safely")

// An auxiliary node that cannot belong to a hypergraph
class FreeNode(arity: Int) extends Node(arity) {
  override def uniqueName: String =
    "{" + super.uniqueName + "}"
    
  override def outs = throw new InvalidFreeNodeUseException
  override def ins = throw new InvalidFreeNodeUseException
  override def outsMut = throw new InvalidFreeNodeUseException
  override def insMut = throw new InvalidFreeNodeUseException
}
