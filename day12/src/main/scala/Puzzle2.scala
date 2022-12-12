import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle2
{
    def findPath[A](surface: Array[Array[A]], start: (Int, Int), 
        endPredicate: ((Int, Int)) => Boolean, movePredicate: (A, A) => Boolean): List[(Int, Int)] =
    {
        val width = surface(0).size
        val height = surface.size

        def neighbors(node: (Int, Int)): Queue[(Int, Int)] =
        {
            Queue((node._1 - 1, node._2),
                (node._1, node._2 + 1),
                (node._1 + 1, node._2),
                (node._1, node._2 - 1))
                .filter(e => (e._1 >= 0) && (e._2 >= 0)
                    && (e._1 < width) && (e._2 < height))
        }

        @tailrec
        def computePath(node: (Int, Int), path: Map[(Int, Int), (Int, Int)],
            parsed: List[(Int, Int)]): List[(Int, Int)] =
        {
            if (node == start)
                parsed
            else
                computePath(path.getOrElse(node, start), path, node :: parsed)
        }
        @tailrec
        def search(left: Queue[(Int, Int)], visited: Set[(Int, Int)],
            path: List[((Int, Int), (Int, Int))]): List[(Int, Int)] =
        {
            left match {
                case h +: t => {
                    if (endPredicate(h))
                        computePath(h, path
                            .groupBy(t => t._1)
                            .mapValues(_.head._2),
                            List())
                    else if (visited.contains(h))
                        search(t, visited, path)
                    else
                    {
                        val nb = neighbors(h)
                            .filter(e => movePredicate(
                                surface(h._2)(h._1), surface(e._2)(e._1)))
                            .filterNot(visited.contains(_))
                        search(t ++ nb, visited + h, nb.map(e => (e, h)) ++: path)
                    }
                }
                case _ => List()
            }
        }

        search(Queue(start), Set(), List())
    }

    def main(args: Array[String]): Unit =
    {
        val rawData = io.Source
            .fromResource("input.txt")
            .getLines
            .map(_.toList)
            .toList
        
        val surface: Array[Array[Char]] = rawData
            .map(_.map(_ match {
                case 'S' => 'a'
                case 'E' => 'z'
                case c => c
            }).toArray)
            .toArray
        val start = rawData
            .zipWithIndex
            .find(t => t._1.contains('E'))
            .map(t => (t._1
                    .zipWithIndex
                    .find(_._1 == 'E')
                    .get
                    ._2, t._2))
            .get

        val path = findPath(surface, start, e => (surface(e._2)(e._1) == 'a'),
            (c: Char, n: Char) => n >= (c - 1))
        println(path.size)
    }
}