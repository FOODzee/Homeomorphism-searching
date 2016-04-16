import graphOps.HomeBuilding._

import scalax.collection.Graph
import scalax.collection.GraphPredef._

/**
 * @author foodzee.
 */
object Main extends App {

  /**
   * Check given graphs for homeomorphism.
   * Try to build the one, if it can exist.
   * Print found homeomorphism or explanation why it wasn't built.
   */
  def check(g1: UnDiGraph, g2: UnDiGraph) = {
    val (can, msg) = g1 canBeHomeomorphicTo g2

    println("=============")
    println(s"g1 = $g1")
    println(s"g2 = $g2")
    if (can) {
      val (home, msg) = findHomeomorphism(g2, g1)
      println(msg)
      if (home.nonEmpty) println(home.get)
    } else println(msg)
    println("=============")
  }

  /**
   * Examples.
   */
  check(Graph(1~2,2~3,3~1), Graph(1~2,2~3,3~4,4~5,5~6,6~7,7~1))

  check(Graph(1~4,1~5,1~6,2~4,2~5,2~6,3~4,3~5,3~6),
        Graph(1~2,1~3,1~4,2~3,2~5,3~6,4~5,4~6,5~6))

  check(Graph(1~2,2~3,3~1), Graph(1~2,2~3,3~1,4~1))

  check(Graph(1~2,1~3,2~4,3~4,4~5,5~6,5~7),
        Graph(1~2,1~3,2~3,3~4,4~5,4~6,6~7))

  check(Graph(1~2,2~3,3~4,4~5,5~6,6~7,7~8,8~1,4~8),
        Graph(1~2,2~3,3~4,4~1,2~5,5~6,6~4))

  check(Graph(1~2,2~3,3~1, 4~5,5~6,6~7,7~8),
        Graph(1~2,2~3,3~4,4~1, 5~6,6~7))

  check(Graph(1~2,2~3, 4~5,5~6,6~7,7~8),
        Graph(1~2,2~3,3~4,4~1, 5~6,6~7))

}
