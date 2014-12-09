import java.util

import com.sun.jna.{Native, Memory, Pointer, Structure}

import scala.collection.{mutable, GenSet, GenSeq}
import scala.collection.JavaConverters._
import scala.util.control.Breaks._
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import sna.Library


/**
 * @author foodzee.
 */
object Utils {

  implicit class Gr(g: Graph[Int, UnDiEdge]) {

    /**
     * Check whether g is homeomorphic to g2.
     */
    def homeomorphicTo(g2: Graph[Int, UnDiEdge]): Boolean = {
      if (g.getCCN != g2.getCCN) false
      else if (g.getCCN != 1) ??? // TODO: some sort of recursion.
      else if (g.graphSize - g.order != g2.graphSize - g2.order) false
      else if (!(g degEq g2)) false
      else if (! (g.smoothed isomorphicTo g2.smoothed)) false
      else true
    }

    /**
     * Check whether g is isomorphic to g2
     */
    def isomorphicTo(g2: Graph[Int, UnDiEdge]): Boolean = {
      val nautyLib = Library("nauty-scala-interface")
      val sg = new sparsegraph(g)
      val sg2 = new sparsegraph(g2)
      nautyLib.isomorphic(sg.ByReference,sg2.ByReference)[Boolean]
    }

    /**
     * @return Graph with no simple paths of length >1
     */
    def smoothed: Graph[Int, UnDiEdge] = {
      val s: scalax.collection.mutable.Graph[Int, UnDiEdge] = scalax.collection.mutable.Graph[Int, UnDiEdge]()
      s ++= g

      def edges = s.edges filter (e => (e._1.degree == 2) || (e._2.degree == 2))
      var skipList: GenSet[s.EdgeT] = GenSet()
      def openEdges = edges diff skipList

      while (openEdges.nonEmpty) {
        val uv = openEdges.head
        val u = uv._1
        val v = uv._2

        val vw = (v.edges - uv).headOption
        val uw = (u.edges - uv).headOption

        breakable {
          // Check whether we found a 3-cycle
          if (vw.nonEmpty && uw.nonEmpty) {
            val w1 = (uw.get.nodeSeq diff GenSeq(u)).head
            val w2 = (vw.get.nodeSeq diff GenSeq(v)).head
            if (w1 == w2) {
              skipList = skipList + uv
              break()
            }
          }

          // If not, select a node to remove from graph
          if (vw.nonEmpty) {
            val w = (vw.get.nodeSeq diff GenSeq(v)).head
            s -= uv
            s -= v
            s.addEdge(u, w)(UnDiEdge)
          }
          else if (uw.nonEmpty) {
            val w = (uw.get.nodeSeq diff GenSeq(u)).head
            s -= uv
            s -= u
            s.addEdge(v, w)(UnDiEdge)
          }
          else {
            skipList = skipList + uv
          }
        }
      }

      s
    }

    /**
     * Obtain the number of connected components of graph
     */
    def getCCN: Int = {
      if (g.isConnected) 1 else 1 + (g diff g.buildBFSTree).getCCN
    }

    /**
     * Obtain some connected component of graph
     */
    def getCC: Graph[Int, UnDiEdge] = g diff (g diff g.buildBFSTree)

    /**
     * Obtain set of all the connected components of graph
     */
    def getCCs: Set[Graph[Int, UnDiEdge]] = {
      if (g.isConnected) Set(g) else Set(g.getCC) union (g diff g.getCC).getCCs
    }

    /**
     * Build BFS tree for given graph.
     */
    def buildBFSTree: Graph[Int, UnDiEdge] = {
      val v0 = g.nodes.head
      var tree = Graph[Int, UnDiEdge]()
      tree += v0
      var back = Graph[Int, UnDiEdge]()
      val Q = mutable.Queue(v0)
      val N0 = mutable.Map(v0 -> 1)
      var k = 1

      while (Q.nonEmpty) {
        val x = Q.front
        def yOpt = (x.neighbors filterNot ((tree union back).nodes.contains(_))).headOption
        while (yOpt.isDefined) {
          val y = yOpt.get
          val e = (x.edges filter (_._1 == x) filter (_._2 == y)).head
          if (N0.contains(y))
            back += e
          else {
            tree += e
            Q.enqueue(y)
            k += 1
            N0(y) = k
          }
        }
        Q.dequeue()
      }
      tree
    }

    /**
     * Calculate number of k-degree nodes
     */
    def degKCount(k: Int): Int = {
      val count = g.degreeNodesMap.get(k)
      if (count.nonEmpty) count.head.size else 0
    }

    /**
     * Check whether the graphs g and g2 are degree-equal, i.e.
     * forall k != 2 g.degKCount == g2.degKCount
     */
    def degEq(g2: Graph[Int, UnDiEdge]): Boolean = ((0 to g.maxDegree) diff GenSeq(2)) forall (k => g.degKCount(k) == g2.degKCount(k))
  }


  /**
   * Structure representing graph to be passed to nauty.
   */
  class sparsegraph extends Structure {
    var nv  : Int         = 0     // the number of vertices
    var nde : Int         = 0     // the number of directed edges
    var v   : Pointer     = Pointer.NULL  // array of vertices' neighbors
    var d   : Pointer     = Pointer.NULL  // array of vertices' degrees
    var e   : Pointer     = Pointer.NULL  // array of edges
    var w   : Pointer     = Pointer.NULL  // unused
    // actual length of arrays
    var vlen: Int         = 0
    var dlen: Int         = 0
    var elen: Int         = 0
    var wlen: Int         = 0 // unused

    override def getFieldOrder = List("nv","nde","v","d","e","w","vlen","dlen","elen","wlen").asJava
    object ByReference extends sparsegraph with Structure.ByReference {}

    /**
     * @param g Graph to be converted to sparse representation.
     */
    def this(g: Graph[Int, UnDiEdge]) = {
      this()
      nv = g.order
      nde = 2 * g.graphSize // Number of directed edges == 2*number of undirected edges

      // Allocate memory for arrays
      val intSize = Native.getNativeSize(Integer.TYPE)
      v = new Memory(nv * intSize)
      d = new Memory(nv * intSize)
      e = new Memory(nde * intSize)
      vlen = nv
      dlen = nv
      elen = nde

      // Fill in d[] and build v-i mappings
      val vimap = mutable.Map[Graph[Int, UnDiEdge]#NodeT, Int]() // vertex-to-integer mapping
      val ivmap = mutable.Map[Int, Graph[Int, UnDiEdge]#NodeT]() // integer-to-vertex mapping

      val iter = g.nodes.iterator
      for (i <- 0 until nv) {
        val v = iter.next()
        d.setInt(i * intSize, v.degree)

        val vi = (v, i)
        vimap += vi
        val iv = (i, v)
        ivmap += iv
      }
      assert(! iter.hasNext)

      // Fill in v[] and e[]
      var j = 0
      for (i <- 0 until nv) {
        v.setInt(i * intSize, j)
        val u = ivmap.get(i).get
        for (w <- u.neighbors) {
          val k = vimap.get(w).get
          e.setInt(j * intSize, k)
          j += 1
        }
      }
    }
  }
}
