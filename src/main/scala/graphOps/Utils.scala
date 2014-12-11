package graphOps

import graphOps.HomeBuilding.UnDiGraph

import scala.collection.{GenSeq, GenSet, mutable}
import scala.util.control.Breaks._
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

/**
 * @author foodzee.
 */
private[graphOps] object Utils {

  implicit class Gr(g: UnDiGraph) {

    /**
     * Smooth given graph.
     *
     * @return Graph with no simple paths of length > 1.
     */
    def smoothed: UnDiGraph = {
      val s = scalax.collection.mutable.Graph[Int, UnDiEdge]()
      s ++= g

      def edges = s.edges filter (e => (e._1.degree == 2) || (e._2.degree == 2))
      var skipList: GenSet[s.EdgeT] = GenSet()
      def openEdges = edges diff skipList

      while (openEdges.nonEmpty) {
        val uv = openEdges.head
        val u = uv._1
        val v = uv._2

        val vw = if (v.degree == 2) (v.edges - uv).headOption else None
        val uw = if (u.degree == 2) (u.edges - uv).headOption else None

        breakable {
          // Check whether we found a 3-cycle.
          if (vw.nonEmpty && uw.nonEmpty) {
            val w1 = (uw.get.nodeSeq diff GenSeq(u)).head
            val w2 = (vw.get.nodeSeq diff GenSeq(v)).head
            if (w1 == w2) {
              skipList = skipList + uv
              break()
            }
          }

          // If not, select a node to remove from graph.
          if (vw.nonEmpty) {
            val w = (vw.get.nodeSeq diff GenSeq(v)).head
            s -= v
            s.addEdge(u, w)(UnDiEdge)
          }
          else if (uw.nonEmpty) {
            val w = (uw.get.nodeSeq diff GenSeq(u)).head
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
     * Calculate number of k-degree nodes.
     */
    def degKCount(k: Int): Int = {
      val count = g.degreeNodesMap.get(k)
      if (count.nonEmpty) count.head.size else 0
    }

    /**
     * Check whether the graphs g and g2 are degree-equal, i.e.
     *  forall k != 2 g.degKCount == g2.degKCount.
     */
    def degEq(g2: UnDiGraph): Boolean =
      ((0 to g.maxDegree) diff GenSeq(2)) forall (k => g.degKCount(k) == g2.degKCount(k))

    /**
     * Obtain the number of connected components of graph.
     */
    def getCCN: Int = if (g.isConnected) 1 else 1 + (g diff g.buildBFSTree).getCCN

    /**
     * Obtain some connected component of graph.
     */
    def getCC: UnDiGraph = g diff (g diff g.buildBFSTree)

    /**
     * Obtain sequence of all connected components of graph.
     */
    def getCCs: GenSeq[UnDiGraph] = if (g.isConnected) GenSeq(g) else GenSeq(g.getCC) union (g diff g.getCC).getCCs

    /**
     * Build BFS tree for given graph.
     */
    def buildBFSTree: UnDiGraph = {
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
          val e = (x.edges filter (e => ((e._1 == x) && (e._2 == y)) || ((e._1 == y) && (e._2 == x)))).head
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
  }


  var lastUsed: Int = 0

  implicit class mutGr(val g: scalax.collection.mutable.Graph[Int, UnDiEdge]) {
    /**
     * Add new vertex to graph and return its number.
     *
     * @return Value of added vertex.
     */
    def newVertex: Int = {
      assert(g.add(lastUsed-1))
      lastUsed -= 1
      lastUsed
    }
  }


  implicit class Matrix(m: Array[Array[Int]]) {
    /**
     * Calculate determinant of square matrix by triangulating it.
     */
    def det: Int = {
      assert(m.length == m(0).length)
      var det = 1
      val N = m.length

      for (i <- 0 until N) {
        val row = rowWithMaxInCol(i,i)
        if (row == -1) det = 0
        else {
          det *= m(row)(i)
          if (i != row) swapRows(i, row)

          for (ii <- i + 1 until N) {
            val tmp = m(ii)(i)
            for (j <- i until N)
              m(ii)(j) -= m(i)(j) * tmp
          }
        }
      }

      /**
       * Find row with maximal element in given column.
       *
       * @param start row to start from;
       * @param col column to search in;
       * @return index of row or -1 if the row not found.
       */
      def rowWithMaxInCol(start: Int, col: Int) = {
        var i = start
        var row = start

        while (i < N && m(i)(col) == 0) i += 1

        if (i == N) -1
        else {
          var max = m(i)(col)
          row = i

          while (i < N) {
            if (m(i)(col) != 0 && max < m(i)(col)) {
              max = m(i)(col)
              row = i
            }
            i += 1
          }

          row
        }
      }

      def swapRows(r1: Int, r2: Int) = {
        for (j <- 0 until N) {
          val tmp = m(r1)(j)
          m(r1)(j) = m(r2)(j)
          m(r2)(j) = tmp
        }
      }

      det
    }
  }
}
