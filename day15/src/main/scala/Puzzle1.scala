import scala.annotation.tailrec

object Puzzle1
{
    case class Segment(start: Int, end: Int)
    {
        def contains(n: Int): Boolean = (n >= start) && (n <= end)
        def contains(s: Segment): Boolean = (s.start >= start) && (s.end <= end)
        def intersects(s: Segment): Boolean = (s.start <= end) && (s.end >= start)
        def length = end - start
    }

    def union(segs: List[Segment], seg: Segment): List[Segment] =
    {
        segs match {
            case h :: t => {
                if (h.contains(seg))
                    segs
                else if (h.intersects(seg))
                    Segment(Math.min(h.start, seg.start), Math.max(h.end, seg.end)) :: t
                else if (h.start < seg.start)
                    seg :: h :: t
                else
                    h :: seg :: t 
            }
            case _ => List(seg)
        }
    }

    def main(args: Array[String]): Unit =
    {
        val sensorPat =
            ("Sensor at x=(-?\\d+), y=(-?\\d+): " +
            "closest beacon is at x=(-?\\d+), y=(-?\\d+)").r

        val row = 2000000

        val sensors = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_ match {
                case sensorPat(sx, sy, bx, by) => (sx.toInt, sy.toInt, bx.toInt, by.toInt)
                case _ => throw new IllegalArgumentException
            })

        val segments = sensors
            .map(t => (t._1, ((t._3 - t._1).abs + (t._4 - t._2).abs) - (t._2 - row).abs))
            .map(t => (t._1, t._2))
            .filter(_._2 >= 0)
            .map(t => Segment(t._1 - t._2, t._1 + t._2))
            .sortBy(_.start)
            .foldLeft(List[Segment]())((z, a) => union(z, a))

        val totalSegmentSize = segments.map(_.length + 1).sum
        val beaconCount = sensors
                .filter(_._4 == row)
                .map(_._3)
                .toSet
                .filter(n => segments.exists(_.contains(n)))
                .size
        
        println(totalSegmentSize - beaconCount)
    }
}