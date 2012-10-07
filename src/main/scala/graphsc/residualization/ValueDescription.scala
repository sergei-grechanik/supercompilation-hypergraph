package graphsc
package residualization

// A model of a value for monotonicity checking
sealed trait ValueDescription {
  def less(v: ValueDescription): Boolean
  def access(i: Int): ValueDescription = this match {
    case VDCtr(as) =>
      if(i >= 0 && i < as.size)
        as(i)
      else
        VDCtr(Nil)
    case VDVar(j, subs) => VDVar(j, subs ++ List(i))
    case VDUnknown() => VDUnknown()
  }
}

case class VDCtr(args: List[ValueDescription]) extends ValueDescription {
  override def less(v: ValueDescription): Boolean = v match {
    case VDCtr(as) =>
      val bycoupling =
        if(args.size < as.size)
          true
        else if(args.size > as.size)
          false
        else
          (args zip as).exists { case (a,b) => a less b }
      bycoupling || as.exists(a => this == a || (this less a))
    case VDVar(_, _) => false
    case VDUnknown() => false
  }
}

case class VDVar(i: Int, subobject: List[Int]) extends ValueDescription {
  override def less(v: ValueDescription): Boolean = v match {
    case VDCtr(as) =>
      as.exists(a => this == a || (this less a))
    case VDVar(j, sub) =>
      i == j && sub.size < subobject.size && subobject.take(sub.size) == sub
    case VDUnknown() => false
  }
}

case class VDUnknown() extends ValueDescription {
  override def less(v: ValueDescription): Boolean = false
}
