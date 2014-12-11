package graphOps

import graphOps.IsomorphismChecking.GrI
import graphOps.Utils._

import scala.collection.GenSeq
import scala.util.control.Breaks._
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Object containing two main homeomorphism checking functions.
 *
 * @author foodzee.
 */
object HomeBuilding {
  type UnDiGraph = Graph[Int, UnDiEdge]

  implicit class GrH(g: UnDiGraph) {
    /**
     * Check whether g can be homeomorphic to g2.
     *
     * @return Pair:
     *         <ul>
     *         <li> Boolean == true doesn't guarantee that homeomorphism really exists! </li>
     *         <li> String is a message. </li>
     *         </ul>
     */
    def canBeHomeomorphicTo(g2: UnDiGraph): (Boolean, String) = {
      val k = g.getCCN
      if (k != g2.getCCN)
        (false, "Graphs have different number of connected components.")
      else if (k != 1)
        (nonconCanBeHom(g2), "While checking non-connected graph.")
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
     * Check whether non-connected graphs g and g2 can be homeomorphic.
     */
    private def nonconCanBeHom(g2: UnDiGraph): Boolean = {
      val k = g.getCCN
      assert (k == g2.getCCN)

      val gCCs = g.getCCs
      val g2CCs = g2.getCCs

      // Build matrix of possible homeomorphisms of connected components.
      val m = Array.ofDim[Int](k, k)
      val gIter = gCCs.iterator
      for (i <- 0 until k) {
        val gi = gIter.next()
        val g2Iter = g2CCs.iterator
        for (j <- 0 until k) {
          val g2i = g2Iter.next()
          val (b, msg) = gi canBeHomeomorphicTo g2i
          m(i)(j) = if (b) 1 else 0
        }
      }

      // If it is invertible then it has at least one 1 in each column and hence each gCC has a homeomorphic g2CC.
      m.det != 0
    }
  }

  /**
   * Search for homeomorphism between g1 and g2.
   *
   * @return Pair:
   *         <ul>
   *         <li> Option contains sequence of graphs generated while building the homeomorphism; </li>
   *         <li> String is some message, usually on why the homeomorphism is not found. </li>
   *         </ul>
   */
  def findHomeomorphism(g1: UnDiGraph, g2: UnDiGraph): (Option[GenSeq[UnDiGraph]], String) = {

    val startTime = System.currentTimeMillis()

    // Feel free to adjust this two parameters.
    val timeout = 10*60*1000 // 10 minutes
    val maxRecursion = (g1.order + g2.order) / 2

    lastUsed = 0

    /**
     * Recursively search for homeomorphism.
     *
     * @param built Already constructed sequence;
     * @return Resulting homeomorphism or None with explaining message.
     */
    def buildHome(g1: UnDiGraph, g2: UnDiGraph, built: GenSeq[UnDiGraph]) : (Option[GenSeq[UnDiGraph]], String) = {
      if (g1 isomorphicTo g2)
        (Option(built), "Homeomorphism found!")
      else if (System.currentTimeMillis() - startTime > timeout)
        (None, "Timeout exceeded!")
      else if (built.size > maxRecursion)
        (None, "Maximal recursion!")
      else {
        var sm: (Option[GenSeq[UnDiGraph]], String) = (None, "")

        /**
         * Choose what to subdivide.
         */
        def choose() = if (g1.order < g2.order) subdivide(g1, g2) else subdivide(g2,g1)

        /**
         * Subdivide given graph.
         *
         * @param one graph to be subdivided;
         * @param another graph to be passed to recursion unchanged.
         */
        def subdivide(one: UnDiGraph, another: UnDiGraph) = breakable {
          for (e <- one.edges) {
            // Subdivide e
            val u = e._1
            val v = e._2

            val sone = scalax.collection.mutable.Graph[Int, UnDiEdge]()
            sone ++= one

            val w = sone.newVertex
            sone -= e
            sone.addEdge(u, w)(UnDiEdge)
            sone.addEdge(v, w)(UnDiEdge)

            // Recursive call with subdivided edge
            val (seq, msg) = buildHome(sone, another, built union GenSeq(sone))
            sm = (seq, msg)
            if (seq.nonEmpty || (msg equals "Timeout exceeded!")) break()
          }
        }

        choose()
        val (seq, msg) = sm
        if (seq.isEmpty && !(msg equals "Timeout exceeded!")) choose()

        sm
      }
    }

    buildHome(g1, g2, GenSeq(g1, g2))
  }

}
