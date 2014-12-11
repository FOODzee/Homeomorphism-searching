package graphOps

import com.sun.jna.{Memory, Native, Pointer, Structure}
import graphOps.HomeBuilding.UnDiGraph
import sna.Library

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * Interface to native nauty-powered isomorphism checker.
 * 
 * @author foodzee.
 */
object IsomorphismChecking {

  implicit class GrI(g: UnDiGraph) {

    /**
     * Check whether g is isomorphic to g2.
     */
    def isomorphicTo(g2: UnDiGraph): Boolean = {
      val nautyLib = Library("/home/foodzee/lib/libnauty-scala-interface.so")
      val sg = new sparsegraph(g)
      val sg2 = new sparsegraph(g2)
      nautyLib.isomorphic(sg,sg2)[Boolean]
    }
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
    def this(g: UnDiGraph) = {
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
      val vimap = mutable.Map[UnDiGraph#NodeT, Int]() // vertex-to-integer mapping
      val ivmap = mutable.Map[Int, UnDiGraph#NodeT]() // integer-to-vertex mapping

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
}
