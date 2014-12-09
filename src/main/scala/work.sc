import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import Utils.Gr

val g = Graph[Int, UnDiEdge](1~3, 4, 1~5, 3~5, 1~2, 2~3, 6)

g.degKCount(0)

val g2 = Graph[Int, UnDiEdge](1,2,3~5,3~4,4~5,5~7,7~6,6~4)
g degEq g2

val g3 = Graph[Int, UnDiEdge](1~2,2~3,3~4, 4~1)
val g4 = Graph[Int, UnDiEdge](1~2,2~3,3~4, 4~5, 5~6, 6~7,7~1)
g3.smoothed
g4.smoothed
g3 equals g4