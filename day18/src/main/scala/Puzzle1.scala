import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle1
{
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

        println(lavaList.map(cube => neighborOffsets
                .map(o => (cube._1 + o._1, cube._2 + o._2, cube._3 + o._3))
                .filter(o => !lavaSet.contains(o))
                .size)
            .sum)
    }
}