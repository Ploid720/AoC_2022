import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle2
{
    def calculateOutside(shape: Set[(Int, Int, Int)],
        directions: List[(Int, Int, Int)]): Set[(Int, Int, Int)] =
    {
        val minCoords = shape.reduce((z, a) => 
            (z._1.min(a._1), z._2.min(a._2), z._3.min(a._3)))
        val maxCoords = shape.reduce((z, a) => 
            (z._1.max(a._1), z._2.max(a._2), z._3.max(a._3)))
        
        val minBound = (minCoords._1 - 1, minCoords._2 - 1, minCoords._3 - 1)
        val maxBound = (maxCoords._1 + 1, maxCoords._2 + 1, maxCoords._3 + 1)

        @tailrec
        def bfs3d(left: Queue[(Int, Int, Int)],
            visited: Set[(Int, Int, Int)]): 
                Set[(Int, Int, Int)] =
        {
            left match {
                case h +: t => {
                    if (visited.contains(h) 
                        || shape.contains(h)
                        || (h._1 < minBound._1)
                        || (h._2 < minBound._2)
                        || (h._3 < minBound._3)
                        || (h._1 > maxBound._1)
                        || (h._2 > maxBound._2)
                        || (h._3 > maxBound._3))
                        bfs3d(t, visited)
                    else
                        bfs3d(t ++ directions.map(d => 
                                (h._1 + d._1, 
                                h._2 + d._2, 
                                h._3 + d._3)),
                            visited + h)
                }
                case _ => visited
            }
        }

        bfs3d(Queue(minBound), Set())
    }

    def main(args: Array[String]): Unit =
    {
        val lavaList = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.split(","))
            .map(_ match {
                case Array(a, b, c) => (a.toInt, b.toInt, c.toInt)
                case _ => throw new IllegalArgumentException
            })
        val lavaSet = lavaList.toSet

        val neighborOffsets = List(0, 0, 1)
            .permutations
            .flatMap(t => List(t, t.map(-_)))
            .map(_ match {
                case List(a, b, c) => (a, b, c)
                case _ => throw new IllegalArgumentException
            })
            .toList

        val outside = calculateOutside(lavaSet, neighborOffsets)

        println(lavaList.map(cube => neighborOffsets
                .map(o => (cube._1 + o._1, cube._2 + o._2, cube._3 + o._3))
                .filter(o => outside.contains(o))
                .size)
            .sum)
    }
}