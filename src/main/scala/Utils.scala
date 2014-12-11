import com.sun.jna.{Native, Memory, Pointer, Structure}

import scala.collection.{GenSeq, mutable, GenSet}
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
     * @return Pair:
     *         <ul>
     *         <li> Option contains sequence of graphs generated while building the homeomorphism </li>
     *         <li> String is some message, usually on why the homeomorphism is not found </li>
     *         </ul>
     */
    def canBeHomeomorphicTo(g2: Graph[Int, UnDiEdge]): (Boolean, String) = {
      val k = g.getCCN
      if (k != g2.getCCN)
        (false, "Graphs have different number of connected components.")
      else if (k != 1) {
        val gCCs = g.getCCs
        val g2CCs = g2.getCCs
        val gIter = gCCs.iterator

        // Build matrix of possible homeomorphisms of connected components.
        val m = Array.ofDim[Int](k,k)
        for (i <- 0 until k) {
          val gi = gIter.next()
          val g2Iter = g2CCs.iterator
          for (j <- 0 until k) {
            val g2i = g2Iter.next()
            val (b, msg) = gi canBeHomeomorphicTo g2i
            m(i)(j) = if (b) 1 else 0
          }
        }

        // Check if there is at least one possible homeomorphism of entire graphs.
        // To do it, make the matrix triangle.
        (m.det != 0, "While checking non-connected graph.")
      }
      else if (g.graphSize - g.order != g2.graphSize - g2.order)
        (false, "A necessary condition broken.")
      else if (!(g degEq g2))
        (false, "Graphs are not degree-equivalent.")
      else if (! (g.smoothed isomorphicTo g2.smoothed))
        (false, "Graphs cannot be smoothed to isomorphic ones.")
      else
        (true, "But not for sure, so you must call findHomeomorphism!")
    }

    /**
     * Check whether g is isomorphic to g2
     */
    def isomorphicTo(g2: Graph[Int, UnDiEdge]): Boolean = {
      val nautyLib = Library("/home/foodzee/lib/libnauty-scala-interface.so")
      val sg = new sparsegraph(g)
      val sg2 = new sparsegraph(g2)
      nautyLib.isomorphic(sg,sg2)[Boolean]
    }

    /**
     * @return Graph with no simple paths of length >1
     */
    def smoothed: Graph[Int, UnDiEdge] = {
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
     * Obtain sequence of all the connected components of graph
     */
    def getCCs: GenSeq[Graph[Int, UnDiEdge]] = {
      if (g.isConnected) GenSeq(g) else GenSeq(g.getCC) union (g diff g.getCC).getCCs
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
    def degEq(g2: Graph[Int, UnDiEdge]): Boolean =
      ((0 to g.maxDegree) diff GenSeq(2)) forall (k => g.degKCount(k) == g2.degKCount(k))
  }


  /**
   * Structure representing graph to be passed to nauty.
   */
  class sparsegraph extends Structure with Structure.ByReference {
    var nv  : Int         = 0     // the number of vertices
    var nde : Long        = 0     // the number of directed edges
    var v   : Pointer     = Pointer.NULL  // array of vertices' neighbors
    var d   : Pointer     = Pointer.NULL  // array of vertices' degrees
    var e   : Pointer     = Pointer.NULL  // array of edges
    var w   : Pointer     = Pointer.NULL  // unused
    // actual length of arrays
    var vlen: Long        = 0
    var dlen: Long        = 0
    var elen: Long        = 0
    var wlen: Long        = 0 // unused

    override def getFieldOrder = List("nde","v","nv","d","e","w","vlen","dlen","elen","wlen").asJava

    /**
     * @param g Graph to be converted to sparse representation.
     */
    def this(g: Graph[Int, UnDiEdge]) = {
      this()
      nv = g.order
      nde = 2 * g.graphSize // Number of directed edges == 2*number of undirected edges

      // Allocate memory for arrays
      val intSize = Native.getNativeSize(Integer.TYPE)
      val longSize = Native.getNativeSize(java.lang.Long.TYPE)
      v = new Memory(nv * longSize)
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

        vimap += ((v, i))
        ivmap += ((i, v))
      }
      assert(! iter.hasNext)

      // Fill in v[] and e[]
      var j = 0
      for (i <- 0 until nv) {
        v.setLong(i * longSize, j)
        val u = ivmap.get(i).get
        for (w <- u.neighbors filter (_ != u)) {
          val k = vimap.get(w).get
          e.setInt(j * intSize, k)
          j += 1
        }
      }
    }
  }

  implicit class Matrix(m: Array[Array[Int]]) {
    def det: Int = {
      var det = 1
      val N = m.length
      def rowWithMaxInCol(start_i: Int, j: Int) = {
        var i = start_i
        var row = i
        while (i < N && m(i)(j) == 0)
          i += 1
        if (i == N) -1
        else {
          var max = m(i)(j)
          row = i

          while (i < N) {
            if (m(i)(j) != 0 && max < m(i)(j)) {
              max = m(i)(j)
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

      det
    }
  }
}
