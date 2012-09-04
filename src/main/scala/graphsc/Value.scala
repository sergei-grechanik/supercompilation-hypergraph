package graphsc

sealed trait Value {
  def size: Int
  def |(v: Value): Value
  def isBottomless: Boolean
}

case class Ctr(constructor: String, args: List[Value]) extends Value {
  override def toString = constructor + " " + args.map("(" + _ + ")").mkString(" ")
  override def size = 1 + args.map(_.size).sum
  
  def |(v: Value): Value = v match {
    case Ctr(c1, a1) if c1 == constructor && a1.length == args.length =>
      Ctr(c1, args zip a1 map { case (l,r) => l | r })
    case Bottom =>
      this
    case ErrorBottom =>
      this
    case _ =>
      throw new Exception("Values are incompatible")
  }
  
  override def isBottomless = args.forall(_.isBottomless)
}

case object Bottom extends Value {
  override def toString = "_|_"
  override def size = 1
  def |(v: Value): Value = v match {
    case ErrorBottom => Bottom
    case v => v
  }
  override def isBottomless = false
}

case object ErrorBottom extends Value {
  override def toString = "_[fail]_"
  override def size = 1
  def |(v: Value): Value = v
  
  override def isBottomless = false
}