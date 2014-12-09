import scala.collection.{GenSet, GenSeq, mutable}
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import util.control.Breaks._

/**
 * @author foodzee.
 */
object Utils {

  implicit class Gr(g: Graph[Int, UnDiEdge]) {

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
    def degEq(g2: Graph[Int, UnDiEdge]): Boolean = ((0 until g.maxDegree + 1) diff GenSeq(2)) forall (k => g.degKCount(k) == g2.degKCount(k))

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
    def isomorphicTo(g2: Graph[Int, UnDiEdge]): Boolean = ???
  }
}
