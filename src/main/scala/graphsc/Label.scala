package graphsc

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