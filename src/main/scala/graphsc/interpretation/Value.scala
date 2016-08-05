package graphsc
package interpretation

sealed trait Value {
  def size: Int
  def isBottomless: Boolean
  def peel: Value = this
  def optimize: Value = this

  def |(x: Value): Value = (this, x) match {
    case (ErrorBottom, v) => v
    case (v, ErrorBottom) => v
    case (Bottom, v) => v
    case (v, Bottom) => v
    case (Nat(i), Nat(j)) if i == j => this
    case (Nat(i), v@Ctr(_,_)) => Nat(i).peel | v
    case (v@Ctr(_,_), Nat(i)) => Nat(i).peel | v
    case (VList(l1), VList(l2)) if l1.size == l2.size => 
      VList((l1, l2).zipped.map{ case (l,r) => l | r })
    case (l@VList(_), v@Ctr(_,_)) => l.peel | v 
    case (v@Ctr(_,_), l@VList(_)) => l.peel | v
    case (Ctr(c1, a1), Ctr(c2, a2)) if c1 == c2 && a1.size == a2.size =>
      Ctr(c1, a1 zip a2 map { case (l,r) => l | r })
    case _ => throw new Exception("Values are incompatible")
  }

  def &(x: Value): Value = (this, x) match {
    case (ErrorBottom, v) => ErrorBottom
    case (v, ErrorBottom) => ErrorBottom
    case (Bottom, v) => Bottom
    case (v, Bottom) => Bottom
    case (Nat(i), Nat(j)) if i == j => this
    case (Nat(i), v) => Nat(i).peel & v
    case (v, Nat(i)) => Nat(i).peel & v
    case (VList(l1), VList(l2)) if l1.size == l2.size =>
      VList((l1, l2).zipped.map{ case (l,r) => l & r })
    case (l@VList(_), v) => (l.peel & v).optimize
    case (v, l@VList(_)) => (l.peel & v).optimize
    case (Ctr(c1, a1), Ctr(c2, a2)) if c1 == c2 && a1.size == a2.size =>
      Ctr(c1, a1 zip a2 map { case (l,r) => l & r })
    case _ => Bottom
  }
}

case class Nat(int: Int) extends Value {
  require(int >= 0)

  override def peel: Value = 
    if(int == 0) Ctr("Z", List())
    else Ctr("S", List(Nat(int - 1)))

  override def toString = int.toString
  override def size = int + 1
  override def isBottomless = true
}

case class VList(list: List[Value]) extends Value {

  override def peel: Value = list match {
    case Nil => Ctr("N", List())
    case h :: t => Ctr("C", List(h, VList(t)))
  }

  override def toString = list.mkString("[", ", ", "]")
  override def size = list.map(_.size + 1).sum + 1
  override def isBottomless = list.forall(_.isBottomless)
}

case class Ctr(constructor: String, args: List[Value]) extends Value {
  override def optimize: Value = constructor match {
    case "S" if args.size == 1 =>
      args(0).optimize match {
        case Nat(i) => Nat(i + 1)
        case _ => this
      }
    case "Z" if args.size == 0 => Nat(0)
    case "C" if args.size == 2 =>
      args(1).optimize match {
        case VList(l) => VList(args(0) :: l)
        case _ => this
      }
    case "N" if args.size == 0 => VList(Nil)
    case _ => this
  }

  override def toString = 
    constructor + " " + args.map("(" + _ + ")").mkString(" ")
  
  override def size = 1 + args.map(_.size).sum
  override def isBottomless = args.forall(_.isBottomless)
}

case object Bottom extends Value {
  override def toString = "_|_"
  override def size = 1
  override def isBottomless = false
}

case object ErrorBottom extends Value {
  override def toString = "_[fail]_"
  override def size = 1
  override def isBottomless = false
}

// Value together with information about minimal cost
case class ValueAndStuff(value: Value, cost: Double, preferred: List[Hyperedge]) {
  def |(other: ValueAndStuff) = {
    val newval = value | other.value
    (newval == value, newval == other.value) match {
      case (true, false) => this
      case (false, true) => other
      case _ =>
        ValueAndStuff(newval, cost min other.cost,
          if(cost < other.cost) preferred
          else if(cost > other.cost) other.preferred
          else (preferred ++ other.preferred).distinct)
    }
  }
  
  def &(other: ValueAndStuff) = {
    val newval = value & other.value
    (newval == value, newval == other.value) match {
      case (true, false) => this
      case (false, true) => other
      case _ =>
        ValueAndStuff(newval, cost min other.cost,
          if(cost < other.cost) preferred
          else if(cost > other.cost) other.preferred
          else (preferred ++ other.preferred).distinct)
    }
  }
}
