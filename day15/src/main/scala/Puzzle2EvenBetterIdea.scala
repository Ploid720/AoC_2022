import scala.annotation.tailrec

object Puzzle2EvenBetterIdea
{
    case class Segment(start: Int, end: Int)
    {
        def contains(n: Int): Boolean = (n >= start) && (n <= end)
        def contains(s: Segment): Boolean = (s.start >= start) && (s.end <= end)
        def intersects(s: Segment): Boolean = (s.start <= end) && (s.end >= start)
        def length = end - start
    }

    def lineIntersection(l1: ((Int, Int), (Int, Int)),
        l2: ((Int, Int), (Int, Int))): Option[(Int, Int)] =
    {
        val s1x: Double = l1._2._1 - l1._1._1;
        val s1y: Double = l1._2._2 - l1._1._2;
        val s2x: Double = l2._2._1 - l2._1._1;
        val s2y: Double = l2._2._2 - l2._1._2;

        val s = (-s1y * (l1._1._1 - l2._1._1) + s1x * (l1._1._2 - l2._1._2)) /
            (-s2x * s1y + s1x * s2y)
        val t = ( s2x * (l1._1._2 - l2._1._2) - s2y * (l1._1._1 - l2._1._1)) /
            (-s2x * s1y + s1x * s2y)

        if ((s >= 0) && (s <= 1) && (t >= 0) && (t <= 1))
            Some((l1._1._1 + (t * s1x)).round.toInt, (l1._1._2 + (t * s1y)).round.toInt)
        else
            None
    }

    def main(args: Array[String]): Unit =
    {
        val sensorPat =
            ("Sensor at x=(-?\\d+), y=(-?\\d+): " +
            "closest beacon is at x=(-?\\d+), y=(-?\\d+)").r

        val sensors = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_ match {
                case sensorPat(sx, sy, bx, by) => (sx.toInt, sy.toInt, bx.toInt, by.toInt)
                case _ => throw new IllegalArgumentException
            })

        val min = 0
        val max = 4000000

        val sensorDists = sensors
            .map(t => (t._1, t._2, (t._3 - t._1).abs + (t._4 - t._2).abs))
            .filter(_._3 >= 0)

        val lines = sensors.map(t => (t._1, t._2, 
                (t._3 - t._1).abs + (t._4 - t._2).abs + 1))
            .flatMap(t => List(
                (t._1, t._2 + t._3),
                (t._1 + t._3, t._2),
                (t._1, t._2 - t._3),
                (t._1 - t._3, t._2),
                (t._1, t._2 + t._3))
                .sliding(2)
                .map(_ match {
                    case List(a, b) => (a, b)
                    case _ => throw new IllegalArgumentException
                }))
            .toSet
        val viablePoints = (for {
            l1 <- lines
            l2 <- lines
            if ((l1 != l2) && lineIntersection(l1, l2).nonEmpty)
        } yield ((l1, l2)))
            .flatMap(t => lineIntersection(t._1, t._2))
            .toList

        viablePoints.filter(p => 
            (p._1 >= min) && (p._1 <= max)
            && (p._2 >= min) && (p._2 <= max)
            && sensorDists.forall(sd => ((p._1 - sd._1).abs + (p._2 - sd._2).abs) > sd._3))
            .foreach(p => println(s"Beacon found: (${p._1}, ${p._2}) - ${p._1 * 4000000l + p._2}"))
    }
}