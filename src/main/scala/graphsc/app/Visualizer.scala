package graphsc.app

import graphsc._
import scala.swing._
import scala.util.Random
import scala.swing.event._
import java.util.concurrent.Semaphore
import java.awt.Dimension
import org.apache.commons.collections15.Transformer
import org.apache.commons.collections15.Predicate
import edu.uci.ics.jung
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.graph.DirectedOrderedSparseMultigraph
import edu.uci.ics.jung.visualization._
import scala.collection.JavaConversions._
import java.awt.event.InputEvent

import VisualizationStuff._
import edu.uci.ics.jung.visualization.layout.ObservableCachingLayout

object VisualizationStuff {
  implicit class TransformerFun[A,B](fun: A => B) extends Transformer[A,B] {
    override def transform(a: A): B = fun(a)
  }
  
  implicit class PredicateFun[A](fun: A => Boolean) extends Predicate[A] {
    override def evaluate(a: A): Boolean = fun(a)
  }
  
  def tr[A,B](fun: A => B) = new TransformerFun(fun)
  def pr[A](fun: A => Boolean) = new PredicateFun(fun)
  
  class Mut[T](var value: T)
  
  type V = Either[Node, Mut[Hyperedge]]
  type E = (Renaming, V, V)
}

trait Visualizer extends Hypergraph with Prettifier {
  def enableVisualizer = true
  
  val graph = new DirectedOrderedSparseMultigraph[V, E]()

  def launchGUI() {
    val mf = new HypergraphMainFrame(this)
    mf.visible = true
  }
  
  def addEdge(ren: Renaming, v1: V, v2: V) {
    graph.addEdge((ren, v1, v2), v1, v2)
  }
  
  def addEdges(v: V) {
    val h = v.right.get.value
    addEdge(h.source.renaming.inv, Left(h.source.node), v)
    for(d <- h.dests)
      addEdge(d.renaming, v, Left(d.node))
  }
  
  def notInteresting(h: Hyperedge): Boolean =
    (h.label == Id() && 
      (h.source.renaming.inv comp h.dests(0).renaming).isId(h.dests(0).node.used)) ||
    (h.label == Let() && h.dests.size == 2 && h.dests(0).getVar.nonEmpty && 
        h.dests(1) ~=~ h.source)
  
  override def onNewHyperedge(h: Hyperedge) {
    if(enableVisualizer && !notInteresting(h)) {
      val v = Right(new Mut(h))
      graph.addVertex(v)
      addEdges(v)
      pause()
    }
    super.onNewHyperedge(h)
  }
  
  override def afterHyperedgeChanged(old: Hyperedge, cur: Hyperedge) {
    if(enableVisualizer) {
      val already = 
        graph.getVertices().exists(_.fold(_ => false, _.value == cur))
      for(Right(m) <- graph.getVertices().toList if m.value == old) {
        if(already || notInteresting(cur)) 
          graph.removeVertex(Right(m))
        else {
          m.value = cur
          for(e <- graph.getIncidentEdges(Right(m)))
            graph.removeEdge(e)
          addEdges(Right(m))
        }
        pause()
      }
    }
    super.afterHyperedgeChanged(old, cur)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    if(enableVisualizer) {
      addEdge(l.renaming, Left(r), Left(l.node))
      pause()
    }
    super.beforeGlue(l, r)
  }
  
  override def afterGlue(n: Node, r: Node) {
    if(enableVisualizer) {
      graph.removeVertex(Left(r))
      pause()
    }
    super.afterGlue(n, r)
  }
  
  abstract override def onNewNode(n: Node) {
    if(enableVisualizer) {
      graph.addVertex(Left(n))
    }
    super.onNewNode(n)
  }
  
  var beingTransformed = Set[Hyperedge]()
  
  override def trans(name: String, hs: Hyperedge*)(body: =>Unit) {
    if(enableVisualizer) {
      val oldBT = beingTransformed 
      beingTransformed = oldBT ++ hs.map(normalize(_))
      pause()
      super.trans(name, hs:_*)(body)
      pause()
      beingTransformed = oldBT.map(normalize(_))
    } else
      super.trans(name, hs:_*)(body)
  }
  
  override def onNameChanged(n: Node, oldname: String) {
    if(enableVisualizer) {

    }
    super.onNameChanged(n, oldname)
  }

  var pausable: Boolean = false
  private val pauseVar = new Semaphore(0)
  
  def pause() {
    if(pausable)
      pauseVar.acquire()
  }
  
  def unpause() {
    synchronized {
      if(pauseVar.availablePermits() == 0)
        pauseVar.release()
    }
  }
}

class HypergraphMainFrame(graph: Visualizer) extends MainFrame {
  
  var hideVarConsts = false
  
  val frl = new FRLayout(graph.graph)
  
  val layout = new SpringLayout(graph.graph, 
      tr((e:E) => if(e._2.isLeft && e._3.isLeft) 20 else 100))
  val view = new VisualizationViewer[V, E](layout)
  
  layout.setSize(new Dimension(10000, 10000))
  frl.setSize(new Dimension(1000, 1000))
  layout.setForceMultiplier(1)
  layout.setStretch(0.2)
  
  view.setBackground(java.awt.Color.WHITE);
  
  private val gm = new control.PluggableGraphMouse()
  gm.add(new control.TranslatingGraphMousePlugin(java.awt.event.InputEvent.BUTTON1_MASK))
  gm.add(new control.ScalingGraphMousePlugin(new control.CrossoverScalingControl(), 0, 1.1f, 0.9f))
  gm.add(new control.PickingGraphMousePlugin(
      InputEvent.BUTTON3_MASK, 
      InputEvent.BUTTON3_MASK | InputEvent.SHIFT_MASK))
  
  view.setGraphMouse(gm)
  
  val sr = new renderers.VertexLabelAsShapeRenderer(view.getRenderContext())
  view.getRenderContext().setVertexShapeTransformer(sr)
  view.getRenderer().setVertexLabelRenderer(sr)
  
  view.getRenderContext().setVertexIncludePredicate(
      pr(p => !hideVarConsts || (p.element match {
        case Left(n) if n.isReal && (n.isVar ||
             n.outs.exists(h => 
               h.dests.isEmpty &&
               (h.label.isInstanceOf[Unused] || h.label.isInstanceOf[Construct]))) =>
          false
        case Right(m) if m.value.label.isInstanceOf[Construct] && m.value.dests.isEmpty =>
          false
        case Right(m) if m.value.label.isInstanceOf[Var] || m.value.label.isInstanceOf[Unused] =>
          false
        case _ => true
      })))
  
  view.getRenderContext().setVertexLabelTransformer(tr{
    case Left(n) =>
      "<html><p>" + n.prettyDebug.replace("\n", "<br>") + "</p></html>"
    case Right(h) =>
      "<html><p>" + h.value.label + "</p></html>"
    })
  
  view.getRenderContext().setVertexFillPaintTransformer(tr{
    case Left(n) => 
      if(n.beingGlued) java.awt.Color.decode("#ffaaaa") 
      else java.awt.Color.decode("#ccccff")
    case Right(m) =>
      if(graph.beingTransformed(m.value)) java.awt.Color.decode("#ffbb33")
      else java.awt.Color.decode("#aaffaa")
    })
      
  val wrapped = Component.wrap(view)
  
  listenTo(wrapped.keys)
  
  reactions += {
    case e:KeyPressed =>
      e.key match {
        case event.Key.N =>
          graph.unpause()
          view.repaint()
        case event.Key.H =>
          hideVarConsts = !hideVarConsts
          view.repaint()
        case event.Key.L =>
          view.setGraphLayout(frl)
          view.repaint()
          layout.setInitializer(frl)
          view.setGraphLayout(layout)
          view.repaint()
        case _ =>
      }
  }
  
  preferredSize = new Dimension(640, 480)
  contents = wrapped
  wrapped.requestFocusInWindow()
}
