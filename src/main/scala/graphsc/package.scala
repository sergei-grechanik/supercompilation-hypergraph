package object graphsc {
  
  def isDefining(h: Hyperedge): Boolean = h.label match {
    case Construct(_) => true
    case Tick() => true
    case Var() => true
    case Unused() => true
    case CaseOf(_) if h.dests(0).getVar.isDefined =>
      (h.dests.tail, h.shifts.tail).zipped.forall(
          (d,sh) => !d.used(h.dests(0).getVar.get + sh))
    case _ => false
  }
  
  def definingHyperedgesNonStrict(n: Node): List[Hyperedge] = {
    val hypers = 
      n.outsMut.toList.filter(h => h.label match {
        case CaseOf(_) if h.dests(0).getVar.isDefined => true 
        case _ => isDefining(h)
      })
      
    // only caseofs can be multiple (due to unpropagated information)
    // but we cannot guarantee that all the equal def hyperedges has been merged
    // assert(hypers.size <= 1 || hypers.head.label.isInstanceOf[CaseOf])
    assert(hypers.isEmpty || hypers.tail.forall(_.label == hypers.head.label))
    hypers
  }
  
  def nodesOf(h: Hyperedge): List[Node] =
    h.source.deref.node :: h.dests.map(_.deref.node)
  
  def nodesOf(h1: Hyperedge, h2: Hyperedge): List[Node] =
    nodesOf(h1) ++ nodesOf(h2)
  
  def indent(s: String, ind: String = "  "): String = ind + indent1(s, ind)
  def indent1(s: String, ind: String = "  "): String = s.replace("\n", "\n" + ind)
}