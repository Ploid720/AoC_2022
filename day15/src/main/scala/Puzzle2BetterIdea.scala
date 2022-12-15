import scala.annotation.tailrec

object Puzzle2BetterIdea
{
    case class Segment(start: Int, end: Int)
    {
        def contains(n: Int): Boolean = (n >= start) && (n <= end)
        def contains(s: Segment): Boolean = (s.start >= start) && (s.end <= end)
        def intersects(s: Segment): Boolean = (s.start <= end) && (s.end >= start)
        def length = end - start
    }

    def checkLine(start: (Int, Int), end: (Int, Int),
        sensorDists: List[(Int, Int, Int)], min: Int, max: Int): Option[(Int, Int)] =
    {
        val dirX = (end._1 - start._1).signum
        val dirY = (end._2 - start._2).signum

        @tailrec
        def checkPoint(p: (Int, Int)): Option[(Int, Int)] =
        {
            if (p == end)
                None
            else
            {
                if ((p._1 >= min) && (p._1 <= max)
                    && (p._2 >= min) && (p._2 <= max))
                {
                    val freePoint = sensorDists.forall(sd => 
                        ((p._1 - sd._1).abs + (p._2 - sd._2).abs) > sd._3)
                    if (freePoint)
                        Some(p)
                    else
                        checkPoint((p._1 + dirX, p._2 + dirY))
                }
                else
                    checkPoint((p._1 + dirX, p._2 + dirY))
            }
        }

        checkPoint(start)
    }
    @tailrec
    def checkLines(lines: List[((Int, Int), (Int, Int))],
        sensorDists: List[(Int, Int, Int)], min: Int, max: Int): Option[(Int, Int)] =
    {
        lines match {
            case h :: t => {
                checkLine(h._1, h._2, sensorDists, min, max) match {
                    case Some(ret) => Some(ret)
                    case None => checkLines(t, sensorDists, min, max)
                }
            }
            case _ => None
        }
        
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

        val lines = sensors.map(t => (t._1, t._2, (t._3 - t._1).abs + (t._4 - t._2).abs + 1))
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
            .toList
        
        checkLines(lines, sensorDists, min, max) match {
            case Some(res) => println(s"Beacon found: (${res._1}, ${res._2}) -" +
              s" ${res._1 * 4000000l + res._2}")
            case None => println("Could not find beacon")
        }
    }
}