import scala.annotation.tailrec

object Puzzle2
{
    case class Line(xs: Int, ys: Int, xe: Int, ye: Int)
    {
        def contains(point: (Int, Int)): Boolean =
        {
            if (xs == xe)
                ((point._1 == xs)
                && (point._2 >= Math.min(ys, ye))
                && (point._2 <= Math.max(ys, ye)))
            else
                ((point._2 == ys)
                && (point._1 >= Math.min(xs, xe))
                && (point._1 <= Math.max(xs, xe)))
        }
    }

    def simulateSand(start: (Int, Int), walls: List[Line]): Set[(Int, Int)] =
    {
        val floorLevel = walls
            .map(line => Math.max(line.ys, line.ye))
            .max + 2
        
        val wallSet = walls
            .flatMap(line => {
                if (line.xs == line.xe)
                    (Math.min(line.ys, line.ye) to Math.max(line.ys, line.ye))
                        .map(y => (line.xs, y))
                else
                    (Math.min(line.xs, line.xe) to Math.max(line.xs, line.xe))
                        .map(x => (x, line.ys))
                    
            })
            .toSet

        @tailrec
        def moveSand(curr: (Int, Int), sand: Set[(Int, Int)]): Set[(Int, Int)] =
        {
            List((curr._1, curr._2 + 1),
                (curr._1 - 1, curr._2 + 1),
                (curr._1 + 1, curr._2 + 1))
                .find(pos => (pos._2 < floorLevel)
                    && !(sand.contains(pos) || wallSet.contains(pos))) match {
                    case Some(p) => moveSand(p, sand)
                    case None if (sand.contains(start) || (curr == start)) => sand + curr
                    case None => moveSand(start, sand + curr)
                }
        }

        moveSand(start, Set())
    }

    def main(args: Array[String]): Unit =
    {
        val rockLines = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .flatMap(_.split("->")
                .map(_.trim)
                .map(_.split(","))
                .map(_ match {
                    case Array(a, b) => (a.toInt, b.toInt)
                    case _ => throw new IllegalArgumentException
                })
                .toList
                .sliding(2)
                .map(_ match {
                    case List(a, b) => Line(a._1, a._2, b._1, b._2)
                })
                .toList)

        val sand = simulateSand((500, 0), rockLines)

        println(sand.size)
    }
}