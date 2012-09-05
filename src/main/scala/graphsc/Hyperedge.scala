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
  
case class Error()
  extends Label

case class CaseOf(cases: List[(String, Int)])
  extends Label

case class Let()
  extends Label

// Like Let, but binds variables only to variables
// should be injective, i.e. no gluing of variables
case class Renaming(vector: List[Int])
  extends Label {
  require(vector.filter(_ >= 0).distinct.size == vector.filter(_ >= 0).size )
  
  def apply(i: Int): Int =
    if(i >= 0 && i < vector.size)
      vector(i)
    else
      -1
  
  override def toString: String =
    "Renaming" + "(" + vector.zipWithIndex.map{case (j,i) => i + " = " + j}.mkString(", ") + ")"
}
  
case class Id()
  extends Label
  
case class Tick()
  extends Label
  
case class Improvement()
  extends Label

case class Var(num: Int)
  extends Label {
  require(num >= 0)
}

  
case class Hyperedge(label: Label, source: Node, dests: List[Node]) {
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
    case Var(i) => Set(i)
    case Construct(_) => (Set[Int]() /: dests.map(_.used))(_ | _)
    case Let() => 
      (Set[Int]() /: dests(0).used.collect { 
          case i if i < dests.tail.size => dests.tail(i).used
        })(_ | _)
    case Renaming(vec) => 
      dests(0).used.collect {
          case i if i < vec.size => vec(i)
        }.filter(_ >= 0)
    case CaseOf(cases) =>
      (dests(0).used /: (dests.tail zip cases).map{ 
          case (d,(_,n)) => d.used.map(_ - n).filter(_ >= 0) })(_ | _)
    case Error() => Set()
  }
  
  def normal: Hyperedge = label match {
    case Let() => 
      Hyperedge(label, source, dests.take(dests(0).arity + 1))
    case Renaming(vec) =>
      val dest_used = dests(0).used
      val newvec =
        for((v,i) <- vec.take(dests(0).arity).zipWithIndex) yield
          if(dest_used(i)) v else -1
      Hyperedge(Renaming(newvec), source, dests)
    case _ => this
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
    
  def run(args: List[Value], nodeRunner: (Node, List[Value]) => Value): Value = {
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
      case Renaming(vec) =>
        nodeRunner(dests(0), vec.map(i => if(i >= 0) args(i) else Bottom))
      case Tick() =>
        nodeRunner(dests(0), args)
      case Improvement() =>
        nodeRunner(dests(0), args)
      case Id() => 
        nodeRunner(dests(0), args)
      case Var(i) =>
        args(i)
      case Error() =>
        Bottom
    }
  }
}

class Node(var marity: Int) {
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: Node = null
  
  var prettyDebug = ""
  
  def used: Set[Int] =
    Set(0 until arity:_*)
    
  def arity: Int =
    if(gluedTo == null) marity else realNode.arity
    
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
