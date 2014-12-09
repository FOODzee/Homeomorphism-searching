import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import Utils._

/**
 * @author foodzee.
 */
object Main extends App {
  val g3 = Graph[Int, UnDiEdge](1~2,2~3,3~4,1~4)
  val g4 = Graph[Int, UnDiEdge](1~3,3~2,2~4,4~1)
  val is = g3 isomorphicTo g4
  println(is)
}
