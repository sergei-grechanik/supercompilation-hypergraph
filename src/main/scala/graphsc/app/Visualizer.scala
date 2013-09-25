package graphsc.app

import graphsc._
import scala.swing._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import scala.util.Random
import scala.swing.event._
import com.mxgraph.model._
import com.mxgraph.layout._
import java.util.concurrent.Semaphore
import java.awt.Dimension

trait Visualizer extends Hypergraph with Prettifier {
  def enableVisualizer = true
  
  val node2cell = collection.mutable.Map[Node, mxCell]()
  val hyperedge2cell = collection.mutable.Map[Hyperedge, mxCell]()
  val mxgraph = new mxGraph
  
  mxgraph.setEdgeLabelsMovable(false)
  mxgraph.setConnectableEdges(false)
  mxgraph.setCellsBendable(false)
  mxgraph.setCellsDisconnectable(false)
  mxgraph.setCellsEditable(false)
  mxgraph.setAutoSizeCells(true)
  mxgraph.setDropEnabled(false)
  mxgraph.setSplitEnabled(false)
  mxgraph.setAllowDanglingEdges(false)
  
  def launchGUI() {
    val mf = new HypergraphMainFrame(this)
    mf.visible = true
  }
  
  def myOnNewHyperedge(h: Hyperedge) {
    if((h.label == Let() && h.dests.size == 2 && h.dests(0).getVar.nonEmpty && 
            h.dests(1) == h.source))
      return
      
    if(hyperedge2cell.exists(p => normalize(p._1) == normalize(h))) {
      mxgraph.getModel().beginUpdate()
      myRemoveHyperedge(h)
      mxgraph.getModel().endUpdate()
    }
    
    val parent = mxgraph.getDefaultParent();
    mxgraph.getModel().beginUpdate()
    
    val cells = (h.source :: h.dests).map(n => node2cell(n.node))
    val x = cells.map(_.getGeometry().getX()).sum / cells.size
    val y = cells.map(_.getGeometry().getY()).sum / cells.size
    
    val cell = 
      mxgraph.insertVertex(parent, null, h.label.toString(), 
          x, y, 100, 30, "ROUNDED;fillColor=lightgreen").asInstanceOf[mxCell]
    mxgraph.updateCellSize(cell)
    
    mxgraph.insertEdge(parent, null, h.source.renaming.inv.toString, cells(0), cell)
    for((d,c) <- h.dests zip cells.tail)
      mxgraph.insertEdge(parent, null, d.renaming.toString, cell, c)
    
    hyperedge2cell += (h -> cell)
    mxgraph.getModel().endUpdate()
  }
  
  def myRemoveHyperedge(h: Hyperedge) {
    val hnorm = normalize(h)
    hyperedge2cell.find(p => normalize(p._1) == hnorm) match {
      case None =>
      case Some((h,c)) =>
        for(x <- mxgraph.getEdges(c))
          mxgraph.getModel().remove(x)
        mxgraph.getModel().remove(c)
        hyperedge2cell.remove(h)
    }
  }
  
  override def onNewHyperedge(h: Hyperedge) {
    if(enableVisualizer) {
      myOnNewHyperedge(h)
      pause()
    }
    super.onNewHyperedge(h)
  }
  
  override def afterHyperedgeRemoved(h: Hyperedge) {
    if(enableVisualizer) {
      mxgraph.getModel().beginUpdate()
      myRemoveHyperedge(h)
      mxgraph.getModel().endUpdate()
    }
    super.afterHyperedgeRemoved(h)
  }
  
  override def beforeGlue(l: RenamedNode, r: Node) {
    if(enableVisualizer) {
      val parent = mxgraph.getDefaultParent();
      mxgraph.getModel().beginUpdate()
      val arr = mxgraph.insertEdge(parent, null, l.renaming, node2cell(r), node2cell(l.node),
                  "dashed=1;strokeColor=red")
      val rcell = node2cell(r) 
      mxgraph.setCellStyle("align=left;fillColor=#ff8c69", Array(rcell))
      mxgraph.getModel().endUpdate()
      
      pause()
      
      mxgraph.getModel().beginUpdate()
      for(h <- r.ins ++ r.outs)
        myRemoveHyperedge(h)
      for(c <- mxgraph.getEdges(rcell))
        mxgraph.getModel().remove(c)
      mxgraph.getModel().remove(rcell)
      mxgraph.getModel().endUpdate()
    }
    super.beforeGlue(l, r)
  }
  
  override def afterGlue(n: Node) {
    if(enableVisualizer) {
      mxgraph.getModel().beginUpdate()
      for(h <- n.ins ++ n.outs)
        myRemoveHyperedge(h)
      mxgraph.getModel().endUpdate()
      for(h <- n.ins ++ n.outs)
        myOnNewHyperedge(h)
      pause()
    }
    super.afterGlue(n)
  }
  
  override def onUsedReduced(n: Node) {
    if(enableVisualizer) {

    }
    super.onUsedReduced(n)
  }
  
  abstract override def onNewNode(n: Node) {
    if(enableVisualizer) {
      val parent = mxgraph.getDefaultParent();
      mxgraph.getModel().beginUpdate()
      val label =
        "used: " + n.used + "\n" + n.uniqueName + "\n" +
        nodeShortProg(n.deref) + " = \n" + n.prettyDebug
      val cell = 
        mxgraph.insertVertex(parent, null, label, 
            Random.nextInt(1000), Random.nextInt(500), 100, 30,
            "align=left").asInstanceOf[mxCell]
      mxgraph.updateCellSize(cell)
      node2cell += (n -> cell)
      mxgraph.getModel().endUpdate()
    }
    super.onNewNode(n)
  }
  
  override def onNameChanged(n: Node, oldname: String) {
    if(enableVisualizer && node2cell.contains(n)) {
      val label =
        "used: " + n.used + "\n" + n.uniqueName + "\n" +
        nodeShortProg(n.deref) + " = \n" + n.prettyDebug
      mxgraph.getModel().beginUpdate()
      val cell = node2cell(n)
      cell.setValue(label)
      mxgraph.updateCellSize(cell)
      mxgraph.getModel().endUpdate()
    }
    super.onNameChanged(n, oldname)
  }
  
  def hyperedge2cellN(h: Hyperedge): Option[mxCell] = {
    hyperedge2cell.find(p => normalize(p._1) == normalize(h)).map(_._2)
  }
  
  /*
  override def logTrans(name: String, hs: Seq[Hyperedge]) {
    if(enableVisualizer) {
      mxgraph.getModel().beginUpdate()
      for(h <- hs; c <- hyperedge2cellN(normalize(h))) {
        mxgraph.setCellStyle("fillColor=orange", Array(c))
      }
      mxgraph.getModel().endUpdate()
      pause()
    }
    super.logTrans(name, hs)
  }
  
  override def logAfterTrans(name: String, hs: Seq[Hyperedge]) {
    if(enableVisualizer) {
      mxgraph.getModel().beginUpdate()
      for(h <- hs; c <- hyperedge2cellN(normalize(h)))
        mxgraph.setCellStyle("fillColor=lightgreen", Array(c))
      mxgraph.getModel().endUpdate()
    }
    super.logAfterTrans(name, hs)
  }*/

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
  val graphComponent = new mxGraphComponent(graph.mxgraph) {
    override def isPanningEvent(event: java.awt.event.MouseEvent): Boolean = true
  }
  
  graphComponent.getViewport().setOpaque(true);
  graphComponent.getViewport().setBackground(java.awt.Color.WHITE);
  graphComponent.setConnectable(false)
  graphComponent.setPanning(true)
  
  val wrapped = Component.wrap(graphComponent)
  
  listenTo(wrapped.mouse.wheel)
  listenTo(wrapped.mouse.clicks)
  listenTo(wrapped.mouse.moves)
  listenTo(wrapped.keys)
  
  //var curpoint: Option[java.awt.Point] = None
  
  reactions += {
    case e:KeyPressed =>
      e.key match {
        case event.Key.L =>
          val layout = new mxFastOrganicLayout(graph.mxgraph)
          graph.mxgraph.getModel().beginUpdate()
          layout.setForceConstant(200)
          layout.execute(graph.mxgraph.getDefaultParent())
          (new mxParallelEdgeLayout(graph.mxgraph)).execute(graph.mxgraph.getDefaultParent())
          graph.mxgraph.getModel().endUpdate()
        case event.Key.P =>
          graph.mxgraph.getModel().beginUpdate()
          (new mxParallelEdgeLayout(graph.mxgraph)).execute(graph.mxgraph.getDefaultParent())
          graph.mxgraph.getModel().endUpdate()
        case event.Key.N =>
          graph.unpause()
        case _ =>
      }
    case MouseWheelMoved(_,_,_,rot) =>
      if(rot > 0) graphComponent.zoomOut()
      else if(rot < 0) graphComponent.zoomIn()
    /*case MousePressed(_,p,_,_,_) =>
      curpoint = Some(p)
    case MouseReleased(_,_,_,_,_) =>
      curpoint = None
    case MouseDragged(_,p,_) =>
      for(p0 <- curpoint) {
        graphComponent.
        curpoint = Some(p)
      }*/
  }
  
  preferredSize = new Dimension(640, 480)
  contents = wrapped
  wrapped.requestFocusInWindow()
}
