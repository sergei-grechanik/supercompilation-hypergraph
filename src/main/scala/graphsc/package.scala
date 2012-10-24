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
      n.outs.filter(h => h.label match {
        case CaseOf(_) if h.dests(0).getVar.isDefined => true 
        case _ => isDefining(h)
      })
      
    // only caseofs can be multiple (due to unpropagated information)
    assert(hypers.size <= 1 || hypers.head.label.isInstanceOf[CaseOf])
    hypers.toList
  }
  
  // Filters out hyperedges with unpropagated information
  def definingHyperedge(n: Node): Option[Hyperedge] = {
    val lst =
      definingHyperedgesNonStrict(n).filter(isDefining(_))
    assert(lst.size <= 1)
    lst.headOption
  }
}