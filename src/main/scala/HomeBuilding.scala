import scala.collection.GenSeq
import scala.util.control.Breaks._
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import Utils._

/**
 * @author foodzee.
 */
object HomeBuilding {
  type UnDiGraph = Graph[Int, UnDiEdge]

  /**
   * Search for homeomorphism between g1 and g2.
   * @return Pair:
   *         <ul>
   *         <li> Option contains sequence of graphs generated while building the homeomorphism </li>
   *         <li> String is some message, usually on why the homeomorphism is not found </li>
   *         </ul>
   */
  def findHomeomorphism(g1: UnDiGraph, g2: UnDiGraph): (Option[GenSeq[UnDiGraph]], String) = {

    val startTime = System.currentTimeMillis()
    val timeout = 10*60*1000 // 10 minutes
    val maxRecursion = (g1.order + g2.order) / 2
    lastUsed = 0

    def buildHome(g1: UnDiGraph, g2: UnDiGraph, built: GenSeq[UnDiGraph]) : (Option[GenSeq[UnDiGraph]], String) = {
      if (g1 isomorphicTo g2)
        (Option(built), "Homeomorphism found!")
      else if (System.currentTimeMillis() - startTime > timeout)
        (None, "Timeout exceeded!")
      else if (built.size > maxRecursion)
        (None, "Maximal recursion!")
      else {
        var sm: (Option[GenSeq[UnDiGraph]], String) = (None, "123")

        /**
         * Subdivide given graph
         * @param one graph to be subdivided
         * @param another graph to be passed to recursion unchanged
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

        def choose() = if (g1.order < g2.order) subdivide(g1, g2) else subdivide(g2,g1)

        choose()
        val (seq, msg) = sm
        if (seq.isEmpty && !(msg equals "Timeout exceeded!"))
          choose()

        sm
      }
    }

    buildHome(g1, g2, GenSeq(g1, g2))
  }

  var lastUsed: Int = 0

  implicit class mutGr(val g: scalax.collection.mutable.Graph[Int, UnDiEdge]) {

    /**
      * @return Unused number for new vertex in g.
     */
    def newVertex: Int = {
      assert(g.add(lastUsed-1))
      lastUsed -= 1
      lastUsed
    }
  }

}
