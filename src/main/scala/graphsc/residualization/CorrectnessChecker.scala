package graphsc
package residualization

trait CorrectnessChecker[S] {
  def apply(n: RenamedNode): S
  def through(s: S, h: Hyperedge): List[S]
  def safe(s: S): Boolean
}

object CorrectnessChecker {
  implicit object GuardedOrMonotoneCorrectnessChecker 
      extends CorrectnessChecker[GuardedOrStructural] {
    type S = GuardedOrStructural
    override def apply(n: RenamedNode): S =
      GuardedOrStructural(
        (0 until n.arity).map(i => if(n.used(i)) VDVar(i, Nil) else VDCtr(Nil)).toList,
        (0 until n.arity).toList.map(VDVar(_, Nil)),
        true, false)
        
    def through(s: S, h: Hyperedge): List[S] = s.through(h)
    def safe(s: S): Boolean = s.safe
  }
}

case class GuardedOrStructural(
    args: List[ValueDescription],
    origargs: List[ValueDescription],
    preguarded: Boolean = true, 
    guarded: Boolean = false) {
  
  def safe: Boolean =
    (VDCtr(args) less VDCtr(origargs)) || guarded
  
  def rename(r: Renaming, as: List[ValueDescription]): List[ValueDescription] = {
    r.vector.map {
      i =>
        if(i >= 0 && i < as.size)
          as(i)
        else
          VDCtr(Nil)
    }
  }
  
  def expand(
      v: ValueDescription, 
      expanded: ValueDescription, 
      where: ValueDescription): ValueDescription = v match {
    case VDVar(i, l) =>
      if(v == where)
        expanded
      else
        where match {
          case VDCtr(lst) => VDCtr(lst.map(expand(v, expanded, _)))
          case _ => where
        }
    case _ => where
  }
  
  def run(n: RenamedNode, as: List[ValueDescription], h: List[Node] = Nil): ValueDescription = {
    if(h.contains(n.node))
      VDUnknown()
    else
      n.getVar match {
        case Some(i) =>
          if(i >= 0 && i < as.size) as(i) else VDCtr(Nil)
        case None =>
          (n.node.outs.find(_.label.isInstanceOf[Construct]) : @unchecked) match {
            case Some(Hyperedge(Construct(_), src, ds)) =>
              val as1 = rename(src.renaming.inv, as)
              VDCtr(ds.map(d => run(d, as1, n.node :: h)))
            case None => VDUnknown()
          }
      }
  }
  
  def through(h: Hyperedge): List[GuardedOrStructural] = {
    val args1 = rename(h.source.renaming.inv, args)
    h.label match {
      case Construct(_) =>
        h.dests.map {
          d =>
            // If the position was preguarded then it is now guarded
            GuardedOrStructural(rename(d.renaming, args1), origargs, preguarded, preguarded)
        }
      case CaseOf(cases) =>
        GuardedOrStructural(rename(h.dests(0).renaming, args1), origargs, false, false) ::
          (h.dests(0).getVar match {
            case Some(i) =>
              val varvd = if(i >= 0 && i < args1.size) args1(i) else VDCtr(Nil)
              (h.dests.tail zip cases).map {
                case (d,(_,n)) =>
                  val vars = (0 until n).map(varvd access _).toList
                  val newargs = vars ++ args1.map(expand(varvd, VDCtr(vars), _))
                  val neworigargs = origargs.map(expand(varvd, VDCtr(vars), _))
                  GuardedOrStructural(
                      rename(d.renaming, newargs), neworigargs, preguarded, guarded)
              }
            case None =>
              (h.dests.tail zip cases).map {
                case (d,(_,n)) =>
                  val newargs = List.fill(n)(VDUnknown()) ++ args1
                  GuardedOrStructural(rename(d.renaming, newargs), origargs, preguarded, guarded)
              }
          })
      case Let() =>
        val newargs = h.dests.tail.map(run(_, args1))
        GuardedOrStructural(rename(h.dests(0).renaming, newargs), origargs, preguarded, guarded) ::
          h.dests.tail.map {
            d =>
              GuardedOrStructural(rename(d.renaming, args1), origargs, false, false)
          }
      case Tick() =>
        List(GuardedOrStructural(
            rename(h.dests(0).renaming, args1), origargs, preguarded, preguarded))
      case Improvement() =>
        List(GuardedOrStructural(
            rename(h.dests(0).renaming, args1), origargs, preguarded, guarded))
      case Id() => 
        List(GuardedOrStructural(
            rename(h.dests(0).renaming, args1), origargs, preguarded, guarded))
      case Var() =>
        Nil
      case Error() =>
        Nil
    }
  }
}
