package graphsc
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import java.lang.RuntimeException

sealed trait Label {
  def isSimple: Boolean = this match {
    case Id() => true
    case Tick() => true
    case Improvement() => true
    case Construct(_) => true
    case Error() => true
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
  
case class Id()
  extends Label
  
case class Tick()
  extends Label
  
case class Improvement()
  extends Label

case class Var()
  extends Label

// Should be injective, i.e. no gluing of variables
case class Renaming(vector: List[Int]) {
  require(vector.filter(_ >= 0).distinct.size == vector.filter(_ >= 0).size)
  
  override def toString: String =
    vector.zipWithIndex.map{case (j,i) => i + " = " + j}.mkString("(", ", ", ")")
  
  def isId =
    vector.zipWithIndex.forall{ case (a,b) => a == b }
  
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
      if(isId) h
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
  def isPlain = renaming.isId
  def plain = RenamedNode.fromNode(node)
  def arity: Int = (used + (-1)).max + 1
  def used: Set[Int] = renaming comp node.used
  def deref = (renaming comp node.deref).normal
  def normal =
    RenamedNode(
        Renaming((0 until node.arity toList).map(i => 
          if(node.used(i)) renaming(i) else -1)).normal, 
        node)
}

object RenamedNode {
  def fromNode(node: Node): RenamedNode = 
    RenamedNode(Renaming(0 until node.arity toList), node).normal
}

case class Hyperedge(label: Label, source: RenamedNode, dests: List[RenamedNode]) {
  // The renaming of source should preserve all used variables
  require(source == null || source.used.size == source.node.used.size)
  //require(source == null || source.used.size >= used.size)
  
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

class Node(initial_used: Set[Int]) {
  var mused = initial_used
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: RenamedNode = null
  
  var prettyDebug = ""
  
  private def error() =
    throw new RuntimeException("Using undereferenced glued node is not a good idea")
    
  def used: Set[Int] =
    if(gluedTo == null) mused else gluedTo.used
    
  def arity: Int =
    (used + (-1)).max + 1
    
  def isReal: Boolean =
    gluedTo == null
    
  def outs: Set[Hyperedge] = 
    if(gluedTo == null) mouts.toSet else error()
  def ins: Set[Hyperedge] = 
    if(gluedTo == null) mins.toSet else error()
  
  def outsMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else error()
  def insMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else error()
    
  // Sometimes the node was glued to some other node...
  // Imperative programming sucks, I know
  def deref: RenamedNode =
    if(gluedTo == null) RenamedNode.fromNode(this)
    else gluedTo.deref
  
  def uniqueName: String =
    if(gluedTo == null)
      super.toString + "/" + arity
    else
      super.toString + "/" + arity + "(" + deref + ")"
    
  override def toString: String =
    uniqueName
}

class InvalidFreeNodeUseException extends 
  Exception("FreeNode should be glued and then dereferenced to be used safely")

// An auxiliary node that cannot belong to a hypergraph
class FreeNode(initial_used: Set[Int]) extends Node(initial_used) {
  override def uniqueName: String =
    "{" + super.uniqueName + "}"
    
  override def outs = throw new InvalidFreeNodeUseException
  override def ins = throw new InvalidFreeNodeUseException
  override def outsMut = throw new InvalidFreeNodeUseException
  override def insMut = throw new InvalidFreeNodeUseException
}
