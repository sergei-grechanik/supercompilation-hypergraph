package graphsc

class Node(initial_used: Set[Int]) {
  var mused = initial_used
  val mouts = collection.mutable.Set[Hyperedge]()
  val mins = collection.mutable.Set[Hyperedge]()
  var gluedTo: RenamedNode = null
  // Indicated that this node is in the process of gluing
  var beingGlued: Boolean = false
  // Defining hyperedge. Once it's defined, it cannot change anymore. 
  private var definingHyperedgeCached: Hyperedge = null
  private var isVarCached: Option[Boolean] = None
  
  var prettyDebug = ""
  
  private def error() =
    throw new RuntimeException("Using undereferenced glued node is not a good idea")
    
  def used: Set[Int] =
    if(gluedTo == null) mused else gluedTo.used
    
  def arity: Int =
    (used + (-1)).max + 1
    
  def isReal: Boolean =
    gluedTo == null
    
  def outs: List[Hyperedge] = 
    if(gluedTo == null) mouts.toList else error()
  def ins: List[Hyperedge] = 
    if(gluedTo == null) mins.toList else error()
  
  def outsMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mouts else error()
  def insMut: collection.mutable.Set[Hyperedge] = 
    if(gluedTo == null) mins else error()
    
  def ~~(n: Node): Boolean =
    if(gluedTo == null && n.gluedTo == null)
      this == n
    else if(gluedTo != null)
      gluedTo.node ~~ n
    else
      n ~~ this
    
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
    
  def isVar: Boolean =
    isVarCached match {
      case Some(b) => b
      case None =>
        for(h <- outsMut) h.label match {
          case Var() =>
            isVarCached = Some(true)
            return true
          case Construct(_) =>
            isVarCached = Some(false)
            return false
          case _ =>
        }
        return false
    }
      
  def definingHyperedge: Option[Hyperedge] =
    if(definingHyperedgeCached != null)
      Some(definingHyperedgeCached)
    else definingHyperedgesNonStrict(this).filter(isDefining(_)) match {
      case Nil => None
      case h :: Nil =>
        definingHyperedgeCached = h
        isVarCached = Some(h.label == Var())
        Some(h)
      case l =>
        l.foreach(System.err.println(_))
        throw new Exception("Multiple strictly defining hyperedges")
    }
      
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
