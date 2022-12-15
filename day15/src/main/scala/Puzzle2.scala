import scala.annotation.tailrec

object Puzzle2
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
    def subtract(segs: List[Segment], seg: Segment): List[Segment] =
    {
        @tailrec
        def helper(left: List[Segment], parsed: List[Segment]): List[Segment] =
        {
            left match {
                case h :: t => {
                    if (seg.contains(h))
                        helper(t, parsed)
                    else if (h.contains(seg))
                    {
                        val s1 = Segment(h.start, seg.start - 1)
                        val s2 = Segment(seg.end + 1, h.end)
                        if ((s1.length >= 0) && (s2.length >= 0))
                            helper(t, s2 :: s1 :: parsed)
                        else if (s1.length >= 0)
                            helper(t, s1 :: parsed)
                        else if (s2.length >= 0)
                            helper(t, s2 :: parsed)
                        else
                            helper(t, parsed)
                    }
                    else if (h.intersects(seg))
                    {
                        if (h.start <= seg.start)
                            helper(t, Segment(h.start, seg.start - 1) :: parsed)
                        else
                            helper(t, Segment(seg.end + 1, h.end) :: parsed)
                    }
                    else
                        helper(t, h :: parsed)
                }
                case _ => parsed.reverse
            }
        }

        helper(segs, List())
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

        (min to max).foreach(y => {
            val positions = sensors
                .map(t => (t._1, ((t._3 - t._1).abs + (t._4 - t._2).abs) - (t._2 - y).abs))
                .map(t => (t._1, t._2))
                .filter(_._2 >= 0)
                .map(t => Segment(t._1 - t._2, t._1 + t._2))
                .sortBy(_.start)
                .foldLeft(List[Segment]())((z, a) => union(z, a))
                .foldLeft(List(Segment(min, max)))((z, a) => subtract(z, a))
            positions.foreach(s => {
                if (s.length == 0)
                    println(s"Free point found: (${s.start}, $y) - ${s.start * 4000000l + y}")
                else
                    println(s"Free segment found: (${s.start}-${s.end}, $y)")
            })
        })
    }
}