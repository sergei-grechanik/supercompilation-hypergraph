package graphsc.app

import graphsc._

import scala.swing._
import scala.util.Random
import scala.swing.event._
import java.util.concurrent.Semaphore
import java.awt.Dimension

import org.apache.commons.collections15.Transformer

import edu.uci.ics.jung
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.graph.DirectedOrderedSparseMultigraph
import edu.uci.ics.jung.visualization._

import scala.collection.JavaConversions._

object VisualizationStuff {
  implicit class TransformerFun[A,B](fun: A => B) extends Transformer[A,B] {
    override def transform(a: A): B = fun(a)
  }
  
  def tr[A,B](fun: A => B) = new TransformerFun(fun)
  
  class Mut[T](var value: T)
  
  type V = Either[Node, Mut[Hyperedge]]
  type E = (Renaming, V, V)
}

import VisualizationStuff._

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
  
  val layout = new SpringLayout(graph.graph, tr((_:E) => 100))
  val view = new VisualizationViewer[V, E](layout)
  
  view.setBackground(java.awt.Color.WHITE);
  
  private val gm = new control.DefaultModalGraphMouse();
  gm.setMode(control.ModalGraphMouse.Mode.TRANSFORMING);
  view.setGraphMouse(gm)
  
  val sr = new renderers.VertexLabelAsShapeRenderer(view.getRenderContext())
  view.getRenderContext().setVertexShapeTransformer(sr)
  view.getRenderer().setVertexLabelRenderer(sr)
  
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
    case Right(h) =>
      java.awt.Color.decode("#aaffaa")
    })
      
  val wrapped = Component.wrap(view)
  
  //listenTo(wrapped.mouse.wheel)
  //listenTo(wrapped.mouse.clicks)
  //listenTo(wrapped.mouse.moves)
  listenTo(wrapped.keys)
  
  reactions += {
    case e:KeyPressed =>
      e.key match {
        case event.Key.N =>
          graph.unpause()
        case _ =>
      }
//    case MouseWheelMoved(_,_,_,rot) =>
//      if(rot > 0) graphComponent.zoomOut()
//      else if(rot < 0) graphComponent.zoomIn()
  }
  
  preferredSize = new Dimension(640, 480)
  contents = wrapped
  wrapped.requestFocusInWindow()
}
