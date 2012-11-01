package graphsc

package object interpretation {
  def runHyperedge(
      h: Hyperedge, args: List[Value], nodeRunner: (RenamedNode, List[Value]) => Value): Value = {
    val Hyperedge(label, source, dests) = h
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
      case Unused() =>
        Bottom
    }
  }
}