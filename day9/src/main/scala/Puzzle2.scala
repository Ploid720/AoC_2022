import scala.annotation.tailrec

object Puzzle2
{
    def moveTail(head: (Int, Int), tail: (Int, Int)): (Int, Int) =
    {
        val xDist = head._1 - tail._1
        val yDist = head._2 - tail._2
        if ((Math.abs(xDist) <= 1) && (Math.abs(yDist) <= 1))
            tail
        else
            (tail._1 + xDist.signum, tail._2 + yDist.signum)
    }
    def step(state: (List[(Int, Int)], List[(Int, Int)]), 
        headMove: (String, Int)): (List[(Int, Int)], List[(Int, Int)]) =
    {
        val offset = headMove match {
            case ("L", _) => (-1, 0)
            case ("R", _) => (1, 0)
            case ("U", _) => (0, -1)
            case ("D", _) => (0, 1)
            case _ => throw new IllegalArgumentException
        }
        
        @tailrec
        def moveKnots(target: (Int, Int), left: List[(Int, Int)],
            parsed: List[(Int, Int)]): (List[(Int, Int)], (Int, Int)) =
        {
            left match {
                case h :: t => {
                    val nh = moveTail(target, h)
                    moveKnots(nh, t, nh :: parsed)
                }
                case _ => (parsed.reverse, parsed.head)
            }
        }

        @tailrec
        def substep(i: Int, lim: Int, knots: List[(Int, Int)], 
            tps: List[(Int, Int)]): (List[(Int, Int)], List[(Int, Int)]) =
        {
            if (i >= lim)
                (knots, tps)
            else
            {
                val head = knots.head
                val newHead = (head._1 + offset._1, head._2 + offset._2)
                val t = moveKnots(newHead, knots.tail, List())
                substep(i + 1, lim, newHead :: t._1, t._2 :: tps)
            }
        }
        
        substep(0, headMove._2, state._1, state._2)
    }

    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .map(s => s.split(" ") match {
                case Array(a, b) => (a, b.toInt)
                case _ => throw new IllegalArgumentException
            })
            .foldLeft((List.fill(10)((0, 0)), List[(Int, Int)]()))((z, m) => step(z, m))
            ._2
            .toSet
            .size)
    }
}